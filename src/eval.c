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

static Value *new_value(enum ValueKind kind)
{
    Value *value = xmalloc(sizeof(Value));

    memset(value, 0, sizeof(Value));
    value->kind = kind;
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
    value->fields = xmalloc((size_t)field_count * sizeof(Value *) + 1);
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

    value->parameter = lambda->local;
    value->body = lambda->body;
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
    Env *env = xmalloc(sizeof(Env));

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

/* The type with its type variables replaced by the types they stand for in
   the environment. */
static Type *close_type(Type *type, const Env *env)
{
    type = prune(type);
    switch (type->kind) {
    case TY_RIGID:
        return force(lookup(env, type->variable))->type;
    case TY_APP:
        return new_type_application(close_type(type->function, env),
                                    close_type(type->argument, env));
    case TY_META:
    case TY_CON:
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
    Value **arguments = xmalloc((size_t)(count + 1) * sizeof(Value *));
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
        Value *equals = make_thunk(pattern->equals, *env);
        Value *literal = make_thunk(pattern->literal, *env);

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
                       apply(make_thunk(pattern->literal, *env), value), env);
    case P_RECORD:
        break;
    }
    return 0;
}

/* The environment extended with the bindings of an E_LET. Every definition
   is a thunk that can see all the bindings, itself included. */
static Env *bind_let(const Expr *let, Env *env)
{
    Env **cells = xmalloc((size_t)let->binding_count * sizeof(Env *) + 1);
    int i;

    for (i = 0; i < let->binding_count; i++) {
        env = extend(env, let->bindings[i], NULL);
        cells[i] = env;
    }
    for (i = 0; i < let->binding_count; i++) {
        cells[i]->value = make_thunk(let->values[i], env);
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
        Value *value = make_thunk(qualifier->expr, *env);

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
static Value *run(Expr *expr, Env *env, Value *value)
{
    for (;;) {
        if (expr != NULL) {
            switch (expr->kind) {
            case E_APP:
                push(F_APPLY, make_thunk(expr->argument, env));
                expr = expr->function;
                continue;
            case E_CASE:
                push(F_CASE, NULL);
                stack[stack_size - 1].expr = expr;
                stack[stack_size - 1].env = env;
                expr = expr->scrutinee;
                continue;
            case E_LAMBDA:
                value = make_closure(expr, env);
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
                value = make_int(expr->number);
                value->tag = expr->big;
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
                die("unresolved expression reached the evaluator");
            }
            expr = NULL;
        } else if (value->kind == V_THUNK) {
            push(F_UPDATE, value);
            value->kind = V_BLACKHOLE;
            expr = value->expr;
            env = value->env;
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
                break;
            case F_CASE:
                env = frame.env;
                expr = choose_alternative(frame.expr, value, &env);
                break;
            case F_APPLY:
                if (value->kind == V_CLOSURE) {
                    env = extend(value->env, value->parameter, frame.value);
                    expr = value->body;
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

/* ---- Exceptions ---- */

/* A catch_exception that is waiting for its function to return. */
typedef struct Handler {
    jmp_buf jump;
    int stack_size; /* of the machine's stack when it started */
    struct Handler *outer;
} Handler;

static Handler *handlers = NULL;
static Value *raised;

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
