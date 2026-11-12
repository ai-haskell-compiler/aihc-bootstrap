#include "eval.h"

#include <setjmp.h>
#include <string.h>

/* The evaluator is a machine with its own stack instead of a recursive C
   function. Deep Haskell recursion therefore cannot overflow the C stack, and
   everything the machine still needs is in `stack`, where a garbage collector
   can find it. (There is no collector yet. When one is added, builtins that
   call force or apply will need a way to keep their own values alive.) */

enum FrameKind {
    F_APPLY,  /* then apply the result to `value` */
    F_UPDATE, /* then store the result in the thunk `value` */
    F_CASE,   /* then choose an alternative of the case `expr` in `env` */
    F_STOP    /* then leave run() */
};

typedef struct Frame {
    enum FrameKind kind;
    Value *value;
    Expr *expr;
    Env *env;
} Frame;

/* A catch_exception that is waiting for its function to return. */
typedef struct Handler {
    jmp_buf jump;
    int stack_size; /* of the machine's stack when it started */
    struct Handler *outer;
} Handler;

static Handler *handlers = NULL;
static Value *raised; /* the exception that raise_exception was given */

static Frame *stack = NULL;
static int stack_size = 0;
static int stack_capacity = 0;

static void push(enum FrameKind kind, Value *value)
{
    if (stack_size == stack_capacity) {
        stack_capacity = stack_capacity * 2 + 1024;
        stack = xrealloc(stack, (size_t)stack_capacity * sizeof(Frame));
    }
    stack[stack_size].kind = kind;
    stack[stack_size].value = value;
    stack[stack_size].expr = NULL;
    stack[stack_size].env = NULL;
    stack_size++;
}

/* A value of the kind, with the memory for the fields that the kind uses:
   see struct Value. */
static Value *new_value(enum ValueKind kind)
{
    size_t size = sizeof(Value);
    Value *value;

    if (kind == V_INT || kind == V_CHAR) {
        size = offsetof(Value, expr);
    } else if (kind == V_CLOSURE) {
        size = offsetof(Value, result);
    } else if (kind == V_THUNK) {
        size = offsetof(Value, field_count);
    } else if (kind == V_CON) {
        size = offsetof(Value, builtin);
    }
    value = gc_alloc(size);
    value->kind = kind;
    return value;
}

/* A ByteString: `length` bytes at `bytes`. An Integer beyond a C long is
   made from one as well (integer.c): it has all the fields. */
Value *make_bytes(const unsigned char *bytes, long length)
{
    Value *value = new_value(V_BYTES);

    value->number = length;
    value->bytes = bytes;
    return value;
}

Value *make_int(long number)
{
    Value *value = new_value(V_INT);

    value->number = number;
    return value;
}

Value *make_char(long code)
{
    Value *value = new_value(V_CHAR);

    value->number = code;
    return value;
}

Value *make_con(int tag, int field_count)
{
    Value *value = new_value(V_CON);

    value->tag = tag;
    value->field_count = field_count;
    value->fields = gc_alloc((size_t)field_count * sizeof(Value *) + 1);
    return value;
}

Value *make_thunk(Expr *expr, Env *env)
{
    Value *value = new_value(V_THUNK);

    value->expr = expr;
    value->env = env;
    return value;
}

Value *make_builtin(const Builtin *builtin)
{
    Value *value = new_value(V_BUILTIN);

    value->builtin = builtin;
    return value;
}

Value *make_io_result(Value *result)
{
    Value *box = make_con(0, 1);

    box->fields[0] = result;
    return box;
}

Value *make_type(Type *type)
{
    Value *value = new_value(V_TYPE);

    value->type = type;
    return value;
}

static Value *make_closure(Expr *lambda, Env *env)
{
    Value *value = new_value(V_CLOSURE);

    value->expr = lambda;
    value->env = env;
    return value;
}

/* The string as a list of characters. */
Value *make_string(const char *string)
{
    Value *list = make_con(TAG_NIL, 0);
    Value **last = &list;

    while (*string != '\0') {
        Value *cell = make_con(TAG_CONS, 2);

        cell->fields[0] = make_char(utf8_decode(&string));
        cell->fields[1] = *last;
        *last = cell;
        last = &cell->fields[1];
    }
    return list;
}

static Env *extend(Env *outer, Binding *binding, Value *value)
{
    Env *env = gc_alloc(sizeof(Env));

    env->binding = binding;
    env->value = value;
    env->outer = outer;
    return env;
}

static Value *lookup(const Env *env, const Binding *binding)
{
    for (; env != NULL; env = env->outer) {
        if (env->binding == binding) {
            return env->value;
        }
    }
    die("variable %s is not in the environment", binding->name);
    return NULL;
}

/* ---- What an expression needs of its environment ---- */

/* An expression that is kept for later (a thunk, a lambda) is kept with
   only the variables it mentions. If it were kept with everything in
   scope, one value that nobody has looked at yet would keep every value
   that was in scope where it was made in use, and those keep others: a
   program that runs in constant memory would not.

   The variables an expression mentions are worked out the first time it is
   kept. The list also has the variables that are bound inside the
   expression; those are not in the environment, and are skipped. */

static void note_expr(Expr *owner, const Expr *expr);

static void note_variable(Expr *owner, Binding *binding)
{
    int i;

    for (i = 0; i < owner->needed_count; i++) {
        if (owner->needed[i] == binding) {
            return;
        }
    }
    owner->needed = xrealloc(owner->needed, (size_t)(owner->needed_count + 1)
                                                * sizeof(Binding *));
    owner->needed[owner->needed_count++] = binding;
}

/* The same types as close_type looks into. */
static void note_type(Expr *owner, Type *type)
{
    type = prune(type);
    if (type->kind == TY_RIGID) {
        note_variable(owner, type->variable);
    } else if (type->kind == TY_APP) {
        note_type(owner, type->function);
        note_type(owner, type->argument);
    }
}

static void note_pattern(Expr *owner, const Pattern *pattern)
{
    int i;

    if (pattern->literal != NULL) {
        note_expr(owner, pattern->literal);
    }
    if (pattern->equals != NULL) {
        note_expr(owner, pattern->equals);
    }
    if (pattern->inner != NULL) {
        note_pattern(owner, pattern->inner);
    }
    for (i = 0; i < pattern->argument_count; i++) {
        note_pattern(owner, pattern->arguments[i]);
    }
}

static void note_expr(Expr *owner, const Expr *expr)
{
    const Alternative *alternative;
    const Guard *guard;
    const Qualifier *qualifier;
    int i;

    switch (expr->kind) {
    case E_LOCAL:
        note_variable(owner, expr->local);
        break;
    case E_APP:
        note_expr(owner, expr->function);
        note_expr(owner, expr->argument);
        break;
    case E_LAMBDA:
    case E_ANNOTATION:
        note_expr(owner, expr->body);
        break;
    case E_LET:
        for (i = 0; i < expr->binding_count; i++) {
            note_expr(owner, expr->values[i]);
        }
        if (expr->body != NULL) {
            note_expr(owner, expr->body);
        }
        break;
    case E_CASE:
        note_expr(owner, expr->scrutinee);
        for (alternative = expr->alternatives; alternative != NULL;
             alternative = alternative->next) {
            note_pattern(owner, alternative->pattern);
            if (alternative->where != NULL) {
                note_expr(owner, alternative->where);
            }
            if (alternative->body != NULL) {
                note_expr(owner, alternative->body);
            }
            for (guard = alternative->guards; guard != NULL; guard = guard->next) {
                for (qualifier = guard->qualifiers; qualifier != NULL;
                     qualifier = qualifier->next) {
                    if (qualifier->let != NULL) {
                        note_expr(owner, qualifier->let);
                        continue;
                    }
                    if (qualifier->pattern != NULL) {
                        note_pattern(owner, qualifier->pattern);
                    }
                    note_expr(owner, qualifier->expr);
                }
                note_expr(owner, guard->body);
            }
        }
        break;
    case E_TYPE:
    case E_METHOD:
        note_type(owner, expr->type);
        break;
    default:
        break;
    }
}

/* An environment with just the variables that the expression mentions. */
static Env *keep_needed(Expr *expr, Env *env)
{
    Env *kept = NULL;
    int i;

    if (!expr->needed_known) {
        note_expr(expr, expr);
        expr->needed_known = 1;
    }
    for (i = 0; i < expr->needed_count; i++) {
        const Env *cell = env;

        while (cell != NULL && cell->binding != expr->needed[i]) {
            cell = cell->outer;
        }
        if (cell != NULL) {
            kept = extend(kept, expr->needed[i], cell->value);
        }
    }
    return kept;
}

/* The value of the expression, to be worked out when something needs it.
   A variable and a lambda need no working out. Neither does a type that
   is a type variable: a function that calls itself passes its own type
   on, and nothing ever looks at it, so a thunk for it would refer to the
   thunk of the call before, and that to the one before it. */
static Value *delay(Expr *expr, Env *env)
{
    if (expr->kind == E_LOCAL) {
        return lookup(env, expr->local);
    }
    if (expr->kind == E_TYPE && prune(expr->type)->kind == TY_RIGID) {
        return lookup(env, prune(expr->type)->variable);
    }
    if (expr->kind == E_LAMBDA) {
        return make_closure(expr, keep_needed(expr, env));
    }
    return make_thunk(expr, keep_needed(expr, env));
}

/* The type with its type variables replaced by the types they stand for in
   the environment. */
static Type *close_type(Type *type, const Env *env)
{
    type = prune(type);
    switch (type->kind) {
    case TY_RIGID:
        return force(lookup(env, type->variable))->type;
    case TY_APP:
        return temporary_application(close_type(type->function, env),
                                     close_type(type->argument, env));
    case TY_META:
    case TY_CON:
    case TY_FORALL:
        break;
    }
    return type;
}

/* The value of a constructor that has not been given any fields yet. */
static Value *make_constructor(const Constructor *constructor)
{
    Value *value;

    if (constructor->arity + constructor->existential_count == 0) {
        return make_con(constructor->tag, 0);
    }
    value = new_value(V_PARTIAL_CON);
    value->constructor = constructor;
    return value;
}

/* Gives a builtin or a constructor one more argument. If that was the last
   one, calls the builtin or builds the constructed value. */
static Value *apply_partial(const Value *function, Value *argument)
{
    int count = function->argument_count;
    Value **arguments = gc_alloc((size_t)(count + 1) * sizeof(Value *));
    Value *result;

    if (count > 0) {
        memcpy(arguments, function->arguments, (size_t)count * sizeof(Value *));
    }
    arguments[count] = argument;
    if (function->kind == V_BUILTIN) {
        if (count + 1 == function->builtin->arity) {
            return function->builtin->function(arguments);
        }
    } else if (count + 1 == function->constructor->arity
                            + function->constructor->existential_count) {
        const Constructor *constructor = function->constructor;
        int i;

        if (constructor->is_newtype) {
            return argument;
        }
        for (i = 0; i < constructor->arity; i++) {
            if (constructor->field_strict != NULL
                && constructor->field_strict[i]) {
                force(arguments[constructor->existential_count + i]);
            }
        }
        result = new_value(V_CON);
        result->tag = function->constructor->tag;
        result->field_count = count + 1;
        result->fields = arguments;
        return result;
    }
    result = new_value(function->kind);
    result->builtin = function->builtin;
    result->constructor = function->constructor;
    result->argument_count = count + 1;
    result->arguments = arguments;
    return result;
}

/* Whether the value has the shape of the pattern. Adds the variables of the
   pattern to `*env`. Sub-patterns evaluate the fields they look at with a
   nested call of the machine; flattening nested patterns in a desugaring
   pass would avoid that. */
static int matches(const Pattern *pattern, Value *value, Env **env)
{
    int skip;
    int i;

    switch (pattern->kind) {
    case P_VARIABLE:
        *env = extend(*env, pattern->variable, value);
        return 1;
    case P_WILDCARD:
        return 1;
    case P_CHAR:
        return force(value)->number == pattern->number;
    case P_LITERAL: {
        Value *equals = delay(pattern->equals, *env);
        Value *literal = delay(pattern->literal, *env);

        return apply(apply(equals, value), literal)->tag == TAG_TRUE;
    }
    case P_CONSTRUCTOR:
        if (pattern->constructor->is_newtype) {
            return matches(pattern->arguments[0], value, env);
        }
        value = force(value);
        if (value->tag != pattern->constructor->tag) {
            return 0;
        }
        /* The types the value stores come before its fields. */
        skip = pattern->constructor->existential_count;
        for (i = 0; i < skip; i++) {
            *env = extend(*env, pattern->existentials[i], value->fields[i]);
        }
        for (i = 0; i < pattern->argument_count; i++) {
            if (!matches(pattern->arguments[i], value->fields[skip + i], env)) {
                return 0;
            }
        }
        return 1;
    case P_AS:
        *env = extend(*env, pattern->variable, value);
        return matches(pattern->inner, value, env);
    case P_BANG:
        return matches(pattern->inner, force(value), env);
    case P_VIEW:
        return matches(pattern->inner,
                       apply(delay(pattern->literal, *env), value), env);
    case P_RECORD:
        break;
    }
    return 0;
}

/* The environment extended with the bindings of an E_LET. Every definition
   is a thunk that can see all the bindings, itself included. */
static Env *bind_let(const Expr *let, Env *env)
{
    Env **cells = gc_alloc((size_t)let->binding_count * sizeof(Env *) + 1);
    int i;

    for (i = 0; i < let->binding_count; i++) {
        env = extend(env, let->bindings[i], NULL);
        cells[i] = env;
    }
    /* A definition may mention any of the bindings, so all of them get
       their value before any of them gets its environment. */
    for (i = 0; i < let->binding_count; i++) {
        cells[i]->value = let->values[i]->kind == E_LAMBDA
            ? make_closure(let->values[i], NULL)
            : make_thunk(let->values[i], NULL);
    }
    for (i = 0; i < let->binding_count; i++) {
        cells[i]->value->env = keep_needed(let->values[i], env);
    }
    return env;
}

/* Whether all qualifiers of the guard hold. Adds the variables of its
   pattern qualifiers to `*env`. Like sub-patterns, guards are evaluated
   with a nested call of the machine. */
static int guard_holds(const Guard *guard, Env **env)
{
    const Qualifier *qualifier;

    for (qualifier = guard->qualifiers; qualifier != NULL;
         qualifier = qualifier->next) {
        Value *value;

        if (qualifier->let != NULL) {
            *env = bind_let(qualifier->let, *env);
            continue;
        }
        value = delay(qualifier->expr, *env);
        if (qualifier->pattern != NULL) {
            if (!matches(qualifier->pattern, value, env)) {
                return 0;
            }
        } else if (force(value)->tag != TAG_TRUE) {
            return 0;
        }
    }
    return 1;
}

/* The body of the alternative of a case expression that the value selects.
   Sets `*env` to the environment for it. */
static Expr *choose_alternative(const Expr *expr, Value *value, Env **env)
{
    const Alternative *alternative;
    const Guard *guard;
    Env *outer = *env;

    for (alternative = expr->alternatives; alternative != NULL;
         alternative = alternative->next) {
        Env *matched = outer;

        if (!matches(alternative->pattern, value, &matched)) {
            continue;
        }
        if (alternative->where != NULL) {
            matched = bind_let(alternative->where, matched);
        }
        if (alternative->body != NULL) {
            *env = matched;
            return alternative->body;
        }
        for (guard = alternative->guards; guard != NULL; guard = guard->next) {
            *env = matched;
            if (guard_holds(guard, env)) {
                return guard->body;
            }
        }
    }
    die("%s:%d: no alternative matches", expr->token->file, expr->token->line);
    return NULL;
}

/* The machine. It is in one of two states:
     - expr != NULL: evaluating `expr` in `env`;
     - expr == NULL: holding `value`, the result of the last evaluation.
   It runs until it holds a fully evaluated value and the top frame is
   F_STOP. */
/* Whether the pattern matches every value without evaluating it. */
static int matches_anything(const Pattern *pattern)
{
    switch (pattern->kind) {
    case P_VARIABLE:
    case P_WILDCARD:
        return 1;
    case P_AS:
        return matches_anything(pattern->inner);
    case P_CONSTRUCTOR:
        return pattern->constructor->is_newtype
            && matches_anything(pattern->arguments[0]);
    default:
        return 0;
    }
}

static Value *run(Expr *expr, Env *env, Value *value)
{
    for (;;) {
        if (expr != NULL) {
            switch (expr->kind) {
            case E_APP:
                push(F_APPLY, delay(expr->argument, env));
                expr = expr->function;
                continue;
            case E_CASE:
                if (matches_anything(expr->alternatives->pattern)) {
                    /* The first pattern does not look at the value, so the
                       value is not evaluated: case e of _ -> ... */
                    expr = choose_alternative(
                        expr, delay(expr->scrutinee, env), &env);
                    continue;
                }
                push(F_CASE, NULL);
                stack[stack_size - 1].expr = expr;
                stack[stack_size - 1].env = env;
                expr = expr->scrutinee;
                continue;
            case E_LAMBDA:
                value = make_closure(expr, keep_needed(expr, env));
                break;
            case E_CONSTRUCTOR:
                value = make_constructor(expr->constructor);
                break;
            case E_LOCAL:
                value = lookup(env, expr->local);
                break;
            case E_GLOBAL:
                value = global_value(expr->global);
                break;
            case E_INT:
                /* A literal is an Integer. */
                value = expr->big == 2 ? integer_from_literal(expr->string)
                    : expr->big == 1
                        ? integer_from_unsigned((unsigned long)expr->number)
                        : make_int(expr->number);
                break;
            case E_CHAR:
                value = make_char(expr->number);
                break;
            case E_STRING:
                value = make_string(expr->string);
                break;
            case E_LET:
                env = bind_let(expr, env);
                expr = expr->body;
                continue;
            case E_ANNOTATION:
                expr = expr->body;
                continue;
            case E_TYPE:
                value = make_type(close_type(expr->type, env));
                break;
            case E_METHOD:
                value = find_method(expr->global, close_type(expr->type, env));
                break;
            case E_NAME:
            case E_OPERATORS:
            case E_RECORD:
            case E_VISIBLE:
                die("unresolved expression reached the evaluator");
            }
            expr = NULL;
        } else if (value->kind == V_THUNK) {
            expr = value->expr;
            env = value->env;
            if (stack[stack_size - 1].kind == F_UPDATE) {
                /* The thunk on top of the stack is waiting for exactly
                   this value, so this thunk shares its frame: it becomes
                   that thunk. A loop that ends in a `seq` b, where b is a
                   thunk, would otherwise leave a frame for every turn.
                   The waiting thunk takes over the expression: that is
                   what it is put back to if an exception passes, and it
                   lets go of what its own expression could see. */
                Value *waiting = stack[stack_size - 1].value;

                waiting->expr = expr;
                waiting->env = env;
                value->kind = V_INDIRECTION;
                value->result = waiting;
                value->expr = NULL;
                value->env = NULL;
            } else {
                push(F_UPDATE, value);
                value->kind = V_BLACKHOLE;
            }
        } else if (value->kind == V_INDIRECTION) {
            value = value->result;
        } else if (value->kind == V_BLACKHOLE) {
            die("<<loop>>");
        } else {
            /* A finished value: give it to the frame that is waiting. */
            Frame frame = stack[--stack_size];

            switch (frame.kind) {
            case F_STOP:
                return value;
            case F_UPDATE:
                frame.value->kind = V_INDIRECTION;
                frame.value->result = value;
                /* What the expression could see is no longer needed, and
                   must not stay in use because of this. */
                frame.value->expr = NULL;
                frame.value->env = NULL;
                break;
            case F_CASE:
                env = frame.env;
                expr = choose_alternative(frame.expr, value, &env);
                break;
            case F_APPLY:
                if (value->kind == V_CLOSURE) {
                    env = extend(value->env, value->expr->local, frame.value);
                    expr = value->expr->body;
                } else if (value->kind == V_BUILTIN
                           || value->kind == V_PARTIAL_CON) {
                    value = apply_partial(value, frame.value);
                } else {
                    die("applying something that is not a function");
                }
                break;
            }
        }
    }
}

Value *force(Value *value)
{
    push(F_STOP, NULL);
    return run(NULL, NULL, value);
}

Value *apply(Value *function, Value *argument)
{
    push(F_STOP, NULL);
    push(F_APPLY, argument);
    return run(NULL, NULL, function);
}

/* What the machine holds on to between allocations, for the collector:
   its stack, an exception on its way to a handler, and the values of the
   top-level variables. */
void mark_roots(void)
{
    const Global *global;

    gc_mark_range(stack, stack + stack_size);
    gc_mark(raised);
    for (global = all_globals; global != NULL; global = global->all_next) {
        gc_mark(global->value);
    }
}

/* Prints the lines of the program that the innermost frames of the machine
   are waiting at, the innermost first, each file once. */
void print_position(void)
{
    const char *last = NULL;
    int shown = 0;
    int i;

    fprintf(stderr, "  at");
    for (i = stack_size - 1; i >= 0 && shown < 8; i--) {
        const Expr *expr = stack[i].expr;

        if (expr != NULL && expr->token != NULL && expr->token->file != last) {
            last = expr->token->file;
            fprintf(stderr, " %s:%d", last, expr->token->line);
            shown++;
        }
    }
    fprintf(stderr, "\n");
}

/* ---- Exceptions ---- */

Value *catch_exception(Value *function, Value *argument, Value **exception)
{
    Handler handler;
    Value *result;

    handler.stack_size = stack_size;
    handler.outer = handlers;
    handlers = &handler;
    if (setjmp(handler.jump) == 0) {
        result = apply(function, argument);
        handlers = handler.outer;
        return result;
    }
    /* raise_exception jumped here. Drop what the machine was doing. A thunk
       that was being evaluated goes back to being a thunk: evaluating it
       again raises the exception again. */
    handlers = handler.outer;
    while (stack_size > handler.stack_size) {
        stack_size--;
        if (stack[stack_size].kind == F_UPDATE) {
            stack[stack_size].value->kind = V_THUNK;
        }
    }
    *exception = raised;
    return NULL;
}

void raise_exception(Value *exception)
{
    if (handlers == NULL) {
        die("exception outside of any handler");
    }
    raised = exception;
    longjmp(handlers->jump, 1);
}
