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

static Value *new_value(enum ValueKind kind)
{
    Value *value = gc_alloc_value(0);

    value->kind = kind;
    return value;
}

/* Small numbers and characters are made once: the lexer of aihc-parser
   makes a value for every character it reads. They are outside the
   collector's memory, and never change. */
#define SHARED_INT_LOW (-128)
#define SHARED_INT_HIGH 1024
#define SHARED_CHARS 256

static Value shared_ints[SHARED_INT_HIGH - SHARED_INT_LOW];
static Value shared_chars[SHARED_CHARS];

Value *make_unshared_int(long number)
{
    Value *value = new_value(V_INT);

    value->a.number = number;
    return value;
}

Value *make_int(long number)
{
    Value *value;

    if (number < SHARED_INT_LOW || number >= SHARED_INT_HIGH) {
        return make_unshared_int(number);
    }
    value = &shared_ints[number - SHARED_INT_LOW];
    if (value->kind != V_INT || value->a.number != number) {
        value->kind = V_INT;
        value->a.number = number;
    }
    return value;
}

Value *make_char(long code)
{
    Value *value;

    if (code >= 0 && code < SHARED_CHARS) {
        value = &shared_chars[code];
        value->kind = V_CHAR;
        value->a.number = code;
        return value;
    }
    value = new_value(V_CHAR);
    value->a.number = code;
    return value;
}

/* The fields of a constructor that has none. */
static Value *no_fields[1];

/* The fields are in the cell of the value, right after it: one allocation
   instead of two, and they are next to it in memory. */
Value *make_con(int tag, int field_count)
{
    Value *value = gc_alloc_value(field_count);

    value->kind = V_CON;
    value->tag = tag;
    value->a.field_count = field_count;
    value->b.fields = field_count > 0 ? (Value **)(value + 1) : no_fields;
    return value;
}

Value *make_thunk(Expr *expr, Env *env)
{
    Value *value = new_value(V_THUNK);

    value->a.expr = expr;
    value->c.env = env;
    return value;
}

Value *make_builtin(const Builtin *builtin)
{
    Value *value = new_value(V_BUILTIN);

    value->c.builtin = builtin;
    return value;
}

Value *make_io_result(Value *result)
{
    Value *box = make_con(0, 1);

    box->b.fields[0] = result;
    return box;
}

/* Types are never freed, and a type has no variables in it at run time, so
   each has one value, outside the collector's memory. */
Value *make_type(Type *type)
{
    Value *value = type->value;

    if (value == NULL) {
        value = xmalloc(sizeof(Value));
        memset(value, 0, sizeof(Value));
        value->kind = V_TYPE;
        value->b.type = type;
        type->value = value;
    }
    return value;
}

static Value *make_closure(Expr *lambda, Env *env)
{
    Value *value = new_value(V_CLOSURE);

    value->a.parameter = lambda->local;
    value->b.body = lambda->body;
    value->c.env = env;
    return value;
}

/* The string as a list of characters. */
Value *make_string(const char *string)
{
    Value *list = make_con(TAG_NIL, 0);
    Value **last = &list;

    while (*string != '\0') {
        Value *cell = make_con(TAG_CONS, 2);

        cell->b.fields[0] = make_char(utf8_decode(&string));
        cell->b.fields[1] = *last;
        *last = cell;
        last = &cell->b.fields[1];
    }
    return list;
}

/* ---- Environments ---- */

static Shape empty_shape;

#define SHAPE_BUCKETS 65536
static Shape *shapes[SHAPE_BUCKETS];

/* The shape with one more variable. */
static const Shape *child_shape(const Shape *parent, Binding *binding)
{
    size_t bucket = ((size_t)parent / 16 * 31 + (size_t)binding / 16)
                    % SHAPE_BUCKETS;
    Shape *child;

    /* A variable is mostly added to environments of one shape. */
    if (binding->shape_parent == parent) {
        return binding->shape_child;
    }
    for (child = shapes[bucket]; child != NULL; child = child->next) {
        if (child->parent == parent
            && child->bindings[parent->count] == binding) {
            binding->shape_parent = parent;
            binding->shape_child = child;
            return child;
        }
    }
    child = xmalloc(sizeof(Shape));
    child->count = parent->count + 1;
    child->bindings = xmalloc((size_t)child->count * sizeof(Binding *));
    if (parent->count > 0) {
        memcpy(child->bindings, parent->bindings,
               (size_t)parent->count * sizeof(Binding *));
    }
    child->bindings[parent->count] = binding;
    child->parent = parent;
    child->next = shapes[bucket];
    shapes[bucket] = child;
    binding->shape_parent = parent;
    binding->shape_child = child;
    return child;
}

static const Shape *shape_of(const Env *env)
{
    return env != NULL ? env->shape : &empty_shape;
}

/* A new environment of the shape, with the values of `outer` first and
   room for the others. */
static Env *new_env(const Shape *shape, const Env *outer)
{
    Env *env = gc_alloc_env(shape->count);

    if (outer != NULL) {
        memcpy(env->values, outer->values,
               (size_t)outer->shape->count * sizeof(Value *));
    }
    env->shape = shape;
    return env;
}

/* Binds `parameter` to the argument on top of the machine's stack (an
   F_APPLY frame), and the parameters of the lambdas the body starts with
   to the arguments under it, as long as there are some waiting: one new
   environment for all of them, instead of one for each. Sets `*body` to
   what is left to evaluate. */
static Env *bind_waiting(Env *env, Binding *parameter, Expr **body)
{
    const Shape *shape = shape_of(env);
    Expr *start = *body;
    Expr *rest = start;
    Env *bound;
    int first;
    int count = 1;
    int i;

    while (rest->kind == E_LAMBDA && count < stack_size
           && stack[stack_size - 1 - count].kind == F_APPLY) {
        rest = rest->body;
        count++;
    }
    /* Mostly the same shape as the last time. */
    if (start->bind_from == shape && start->bind_count == count) {
        shape = start->bind_to;
    } else {
        const Shape *from = shape;
        Expr *lambda = start;

        shape = child_shape(shape, parameter);
        for (i = 1; i < count; i++) {
            shape = child_shape(shape, lambda->local);
            lambda = lambda->body;
        }
        start->bind_from = from;
        start->bind_to = shape;
        start->bind_count = count;
    }
    /* The arguments stay on the stack, where the collector finds them,
       until they are in the environment. */
    bound = new_env(shape, env);
    first = shape->count - count;
    for (i = 0; i < count; i++) {
        bound->values[first + i] = stack[stack_size - 1 - i].value;
    }
    stack_size -= count;
    *body = rest;
    return bound;
}

/* Where the variable is in an environment of the shape, or -1. The
   innermost one counts, though a variable is bound only once in an
   environment. */
static int position(const Shape *shape, const Binding *binding)
{
    int i;

    for (i = shape->count - 1; i >= 0; i--) {
        if (shape->bindings[i] == binding) {
            return i;
        }
    }
    return -1;
}

static Value *lookup(const Env *env, const Binding *binding)
{
    int i = position(shape_of(env), binding);

    if (i < 0) {
        die("variable %s is not in the environment", binding->name);
    }
    return env->values[i];
}

/* The value of the variable of an E_LOCAL. An expression is mostly
   evaluated in environments of one shape, so where the variable is is
   remembered. */
static Value *lookup_local(Expr *expr, const Env *env)
{
    if (env != NULL && env->shape == expr->lookup_shape) {
        return env->values[expr->lookup_index];
    }
    expr->lookup_index = position(shape_of(env), expr->local);
    if (expr->lookup_index < 0) {
        die("variable %s is not in the environment", expr->local->name);
    }
    expr->lookup_shape = shape_of(env);
    return env->values[expr->lookup_index];
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

static void note_expr(Needed *owner, const Expr *expr);

static void note_variable(Needed *owner, Binding *binding)
{
    int i;

    for (i = 0; i < owner->count; i++) {
        if (owner->bindings[i] == binding) {
            return;
        }
    }
    owner->bindings = xrealloc(owner->bindings, (size_t)(owner->count + 1)
                                                    * sizeof(Binding *));
    owner->bindings[owner->count++] = binding;
}

/* The same types as close_type looks into. */
static void note_type(Needed *owner, Type *type)
{
    type = prune(type);
    if (type->kind == TY_RIGID) {
        note_variable(owner, type->variable);
    } else if (type->kind == TY_APP) {
        note_type(owner, type->function);
        note_type(owner, type->argument);
    }
}

static void note_pattern(Needed *owner, const Pattern *pattern)
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

/* What the alternatives of a case mention. */
static void note_alternatives(Needed *owner, const Expr *expr)
{
    const Alternative *alternative;
    const Guard *guard;
    const Qualifier *qualifier;

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
}

static void note_expr(Needed *owner, const Expr *expr)
{
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
        note_alternatives(owner, expr);
        break;
    case E_TYPE:
    case E_METHOD:
        note_type(owner, expr->type);
        break;
    default:
        break;
    }
}

/* An environment with just the variables in `needed`. Those that are not
   in `env` are bound inside the expression, and are left out. Where the
   others are is worked out for one shape of `env` at a time, which is
   mostly the only one. */
static Env *keep(Needed *needed, Env *env)
{
    const Shape *from = shape_of(env);
    const Shape *to;
    Env *kept;
    int i;

    if (needed->from != from || needed->positions == NULL) {
        to = &empty_shape;
        needed->positions =
            xrealloc(needed->positions, (size_t)(needed->count + 1) * sizeof(int));
        for (i = 0; i < needed->count; i++) {
            int place = position(from, needed->bindings[i]);

            if (place >= 0) {
                needed->positions[to->count] = place;
                to = child_shape(to, needed->bindings[i]);
            }
        }
        needed->from = from;
        needed->to = to;
    }
    to = needed->to;
    if (to->count == 0) {
        return NULL;
    }
    kept = new_env(to, NULL);
    for (i = 0; i < to->count; i++) {
        kept->values[i] = env->values[needed->positions[i]];
    }
    return kept;
}

/* An environment with just the variables that the expression mentions. */
static Env *keep_needed(Expr *expr, Env *env)
{
    if (!expr->needed.known) {
        note_expr(&expr->needed, expr);
        expr->needed.known = 1;
    }
    return keep(&expr->needed, env);
}

/* What a case keeps while its scrutinee is evaluated: the variables its
   alternatives mention. The scrutinee often is the last use of a variable
   (case run p s of ...), and the value of that variable must not stay in
   use until the scrutinee is done. */
static Env *keep_for_alternatives(Expr *expr, Env *env)
{
    if (!expr->alternatives_needed.known) {
        note_alternatives(&expr->alternatives_needed, expr);
        expr->alternatives_needed.known = 1;
    }
    return keep(&expr->alternatives_needed, env);
}

/* The value of the expression, to be worked out when something needs it.
   A variable and a lambda need no working out. Neither does a type that
   is a type variable: a function that calls itself passes its own type
   on, and nothing ever looks at it, so a thunk for it would refer to the
   thunk of the call before, and that to the one before it. */
static Type *close_type(Type *type, const Env *env);

/* The type with every link followed. Type applications are made once, so
   a type without variables in it is one pointer to compare. */
static Type *follow_links(Type *type)
{
    Type *function;
    Type *argument;

    type = prune(type);
    if (type->kind != TY_APP) {
        return type;
    }
    function = follow_links(type->function);
    argument = follow_links(type->argument);
    if (function == type->function && argument == type->argument) {
        return type;
    }
    return new_type_application(function, argument);
}

static int has_variables(Type *type)
{
    switch (type->kind) {
    case TY_RIGID:
        return 1;
    case TY_APP:
        return has_variables(type->function)
               || has_variables(type->argument);
    default:
        return 0;
    }
}

/* The type of an E_TYPE or E_METHOD, worked out the first time. The type
   checker is done with it when it runs: its links do not change. */
static Type *run_type(Expr *expr)
{
    if (expr->run_type_state == 0) {
        expr->run_type = follow_links(expr->type);
        expr->run_type_state = has_variables(expr->run_type) ? 2 : 1;
    }
    return expr->run_type;
}

/* The type of an E_TYPE or E_METHOD with its variables replaced by what
   they stand for in the environment. */
static Type *closed_type(Expr *expr, const Env *env)
{
    Type *type = run_type(expr);

    return expr->run_type_state == 1 ? type : close_type(type, env);
}

/* The method of an E_METHOD. Found once if its type has no variables in
   it, as long as no instances come: find_method keeps the value in use
   meanwhile. */
static Value *method_of(Expr *expr, const Env *env)
{
    Type *type = closed_type(expr, env);

    if (expr->run_type_state != 1) {
        return find_method(expr->global, type);
    }
    if (expr->method == NULL || expr->method_instances != all_instances) {
        Value *method = find_method(expr->global, type);

        if (!method_is_remembered(type)) {
            return method;
        }
        expr->method = method;
        expr->method_instances = all_instances;
    }
    return expr->method;
}

static Value *delay(Expr *expr, Env *env)
{
    switch (expr->kind) {
    case E_LOCAL:
        return lookup_local(expr, env);
    case E_TYPE:
        if (run_type(expr)->kind == TY_RIGID) {
            return lookup(env, run_type(expr)->variable);
        }
        /* What a type's variables stand for are types too, which are
           quickly found: no thunk is worth making for it. */
        return make_type(closed_type(expr, env));
    case E_LAMBDA:
        return make_closure(expr, keep_needed(expr, env));
    case E_CONSTRUCTOR:
        return make_constructor(expr->constructor);
    case E_CHAR:
        return make_char(expr->number);
    case E_INT:
        if (expr->big == 0) {
            return make_int(expr->number);
        }
        break;
    case E_GLOBAL:
        /* A global is worked out the first time it is evaluated, which
           this is not. */
        if (expr->global->value != NULL) {
            return expr->global->value;
        }
        break;
    default:
        break;
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
        return force(lookup(env, type->variable))->b.type;
    case TY_APP: {
        /* Types made here are never freed, so one without variables in it
           is not made again. */
        Type *function = close_type(type->function, env);
        Type *argument = close_type(type->argument, env);

        if (function == type->function && argument == type->argument) {
            return type;
        }
        return new_type_application(function, argument);
    }
    case TY_META:
    case TY_CON:
    case TY_FORALL:
        break;
    }
    return type;
}

/* The value of a constructor that has not been given any fields yet. */
Value *make_constructor(const Constructor *constructor)
{
    Value *value;

    if (constructor->arity + constructor->existential_count == 0) {
        return make_con(constructor->tag, 0);
    }
    value = new_value(V_PARTIAL_CON);
    value->c.constructor = constructor;
    return value;
}

/* How many arguments a builtin or a constructor takes in all. */
static int arity_of(const Value *function)
{
    if (function->kind == V_BUILTIN) {
        return function->c.builtin->arity;
    }
    return function->c.constructor->arity
           + function->c.constructor->existential_count;
}

/* The builtin or constructor given `count` arguments in all, all of them
   in `arguments`, a new array: calls the builtin or builds the value if
   that is all of them. */
static Value *give_arguments(const Value *function, Value **arguments,
                             int count)
{
    Value *result;

    if (count == arity_of(function)) {
        const Constructor *constructor = function->c.constructor;
        int i;

        if (function->kind == V_BUILTIN) {
            return function->c.builtin->function(arguments);
        }
        if (constructor->is_newtype) {
            return arguments[count - 1];
        }
        for (i = 0; i < constructor->arity; i++) {
            if (constructor->field_strict != NULL
                && constructor->field_strict[i]) {
                force(arguments[constructor->existential_count + i]);
            }
        }
        result = new_value(V_CON);
        result->tag = constructor->tag;
        result->a.field_count = count;
        result->b.fields = arguments;
        return result;
    }
    result = new_value(function->kind);
    result->c.builtin = function->c.builtin;
    result->c.constructor = function->c.constructor;
    result->a.argument_count = count;
    result->b.arguments = arguments;
    return result;
}

/* Gives a builtin or a constructor one more argument. */
static Value *apply_partial(const Value *function, Value *argument)
{
    int count = function->a.argument_count;
    Value **arguments = gc_alloc((size_t)(count + 1) * sizeof(Value *));

    if (count > 0) {
        memcpy(arguments, function->b.arguments, (size_t)count * sizeof(Value *));
    }
    arguments[count] = argument;
    return give_arguments(function, arguments, count + 1);
}

/* Gives a builtin or a constructor `argument` and, if the machine's stack
   has the rest of the arguments it needs waiting on top (F_APPLY), those
   too: one array, instead of a partial application for each. */
static Value *apply_waiting(const Value *function, Value *argument)
{
    int count = function->a.argument_count;
    int total = arity_of(function);
    int waiting = total - count - 1;
    Value **arguments;
    int i;

    if (waiting <= 0 || stack_size < waiting) {
        return apply_partial(function, argument);
    }
    for (i = 1; i <= waiting; i++) {
        if (stack[stack_size - i].kind != F_APPLY) {
            return apply_partial(function, argument);
        }
    }
    arguments = gc_alloc((size_t)total * sizeof(Value *));
    if (count > 0) {
        memcpy(arguments, function->b.arguments, (size_t)count * sizeof(Value *));
    }
    arguments[count] = argument;
    for (i = 1; i <= waiting; i++) {
        arguments[count + i] = stack[--stack_size].value;
    }
    return give_arguments(function, arguments, total);
}

/* The variables a pattern binds, collected while it is matched, so that
   they go into one new environment at the end instead of one each. The
   values are on the C stack meanwhile, where the collector sees them. */
#define BINDER_SIZE 16

typedef struct Binder {
    Env *env; /* with the variables bound before these */
    int count;
    Binding *bindings[BINDER_SIZE];
    Value *values[BINDER_SIZE];
} Binder;

/* The environment with every variable bound so far. */
static Env *bound(Binder *binder)
{
    const Shape *shape = shape_of(binder->env);
    int first = shape->count;
    Env *env;
    int i;

    if (binder->count == 0) {
        return binder->env;
    }
    for (i = 0; i < binder->count; i++) {
        shape = child_shape(shape, binder->bindings[i]);
    }
    env = new_env(shape, binder->env);
    for (i = 0; i < binder->count; i++) {
        env->values[first + i] = binder->values[i];
    }
    binder->env = env;
    binder->count = 0;
    return env;
}

static void bind(Binder *binder, Binding *binding, Value *value)
{
    if (binder->count == BINDER_SIZE) {
        bound(binder);
    }
    binder->bindings[binder->count] = binding;
    binder->values[binder->count] = value;
    binder->count++;
}

/* Whether the value has the shape of the pattern. Sub-patterns evaluate
   the fields they look at with a nested call of the machine; flattening
   nested patterns in a desugaring pass would avoid that. */
static int match_into(const Pattern *pattern, Value *value, Binder *binder)
{
    int skip;
    int i;

    switch (pattern->kind) {
    case P_VARIABLE:
        bind(binder, pattern->variable, value);
        return 1;
    case P_WILDCARD:
        return 1;
    case P_CHAR:
        return force(value)->a.number == pattern->number;
    case P_LITERAL: {
        Value *equals = delay(pattern->equals, bound(binder));
        Value *literal = delay(pattern->literal, bound(binder));

        return apply(apply(equals, value), literal)->tag == TAG_TRUE;
    }
    case P_CONSTRUCTOR:
        if (pattern->constructor->is_newtype) {
            return match_into(pattern->arguments[0], value, binder);
        }
        value = force(value);
        if (value->tag != pattern->constructor->tag) {
            return 0;
        }
        /* The types the value stores come before its fields. */
        skip = pattern->constructor->existential_count;
        for (i = 0; i < skip; i++) {
            bind(binder, pattern->existentials[i], value->b.fields[i]);
        }
        for (i = 0; i < pattern->argument_count; i++) {
            if (!match_into(pattern->arguments[i], value->b.fields[skip + i],
                            binder)) {
                return 0;
            }
        }
        return 1;
    case P_AS:
        bind(binder, pattern->variable, value);
        return match_into(pattern->inner, value, binder);
    case P_BANG:
        return match_into(pattern->inner, force(value), binder);
    case P_VIEW:
        return match_into(pattern->inner,
                          apply(delay(pattern->literal, bound(binder)), value),
                          binder);
    case P_RECORD:
        break;
    }
    return 0;
}

/* Whether the value has the shape of the pattern. Adds the variables of the
   pattern to `*env` if it has. */
static int matches(const Pattern *pattern, Value *value, Env **env)
{
    Binder binder;

    binder.env = *env;
    binder.count = 0;
    if (!match_into(pattern, value, &binder)) {
        return 0;
    }
    *env = bound(&binder);
    return 1;
}

/* The environment extended with the bindings of an E_LET. Every definition
   is a thunk that can see all the bindings, itself included. A binding with
   a ! in front is evaluated before the body, as in GHC; a program may rely
   on that to let go of what the expression refers to. */
static Env *bind_let(const Expr *let, Env *env)
{
    const Shape *shape = shape_of(env);
    int first = shape->count;
    int i;

    for (i = 0; i < let->binding_count; i++) {
        shape = child_shape(shape, let->bindings[i]);
    }
    env = new_env(shape, env);
    /* A definition may mention any of the bindings, so all of them get
       their value before any of them gets its environment. */
    for (i = 0; i < let->binding_count; i++) {
        env->values[first + i] = let->values[i]->kind == E_LAMBDA
            ? make_closure(let->values[i], NULL)
            : make_thunk(let->values[i], NULL);
    }
    for (i = 0; i < let->binding_count; i++) {
        Env *kept = keep_needed(let->values[i], env);

        /* A collection in between may have made the thunk old. */
        env->values[first + i]->c.env = kept;
        gc_written(env->values[first + i]);
    }
    for (i = 0; i < let->strict_count; i++) {
        force(lookup(env, let->strict[i]));
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
                stack[stack_size - 1].env = keep_for_alternatives(expr, env);
                expr = expr->scrutinee;
                continue;
            case E_LAMBDA:
                if (stack[stack_size - 1].kind == F_APPLY) {
                    /* The closure would be applied at once: its parameter
                       is bound without making it. A function of several
                       arguments, \a -> \b -> ..., takes them all this
                       way. */
                    Binding *parameter = expr->local;

                    expr = expr->body;
                    env = bind_waiting(env, parameter, &expr);
                    continue;
                }
                value = make_closure(expr, keep_needed(expr, env));
                break;
            case E_CONSTRUCTOR:
                value = make_constructor(expr->constructor);
                break;
            case E_LOCAL:
                value = lookup_local(expr, env);
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
                value = make_type(closed_type(expr, env));
                break;
            case E_METHOD:
                value = method_of(expr, env);
                break;
            case E_NAME:
            case E_OPERATORS:
            case E_RECORD:
            case E_VISIBLE:
                die("unresolved expression reached the evaluator");
            }
            expr = NULL;
        } else if (value->kind == V_THUNK) {
            expr = value->a.expr;
            env = value->c.env;
            if (stack[stack_size - 1].kind == F_UPDATE) {
                /* The thunk on top of the stack is waiting for exactly
                   this value, so this thunk shares its frame: it becomes
                   that thunk. A loop that ends in a `seq` b, where b is a
                   thunk, would otherwise leave a frame for every turn. */
                value->kind = V_INDIRECTION;
                value->b.result = stack[stack_size - 1].value;
                gc_written(value);
            } else {
                push(F_UPDATE, value);
                value->kind = V_BLACKHOLE;
            }
            /* A thunk that is being evaluated lets go of what its
               expression could see: the machine has that now, and lets go
               of each part when it is done with it. A parser that keeps
               its input in a variable would otherwise keep all of it until
               the parse is over. */
            value->a.expr = NULL;
            value->c.env = NULL;
        } else if (value->kind == V_INDIRECTION) {
            value = value->b.result;
        } else if (value->kind == V_BLACKHOLE) {
            die("<<loop>>");
        } else if (value->kind == V_RAISE) {
            raise_exception(value->b.result);
        } else {
            /* A finished value: give it to the frame that is waiting. */
            Frame frame = stack[--stack_size];

            switch (frame.kind) {
            case F_STOP:
                return value;
            case F_UPDATE:
                frame.value->kind = V_INDIRECTION;
                frame.value->b.result = value;
                gc_written(frame.value);
                /* What the expression could see is no longer needed, and
                   must not stay in use because of this. */
                frame.value->a.expr = NULL;
                frame.value->c.env = NULL;
                break;
            case F_CASE:
                env = frame.env;
                expr = choose_alternative(frame.expr, value, &env);
                break;
            case F_APPLY:
                if (value->kind == V_CLOSURE) {
                    stack_size++; /* the argument is taken from there */
                    expr = value->b.body;
                    env = bind_waiting(value->c.env, value->a.parameter, &expr);
                } else if (value->kind == V_BUILTIN
                           || value->kind == V_PARTIAL_CON) {
                    value = apply_waiting(value, frame.value);
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
    /* Most values are evaluated already: the machine is not needed. */
    while (value->kind == V_INDIRECTION) {
        value = value->b.result;
    }
    if (value->kind != V_THUNK && value->kind != V_BLACKHOLE
        && value->kind != V_RAISE) {
        return value;
    }
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
    mark_remembered_methods();
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
       that was being evaluated raises the same exception when it is
       evaluated again, as in GHC: what it would need to start again is
       gone (see run). */
    handlers = handler.outer;
    while (stack_size > handler.stack_size) {
        stack_size--;
        if (stack[stack_size].kind == F_UPDATE) {
            stack[stack_size].value->kind = V_RAISE;
            stack[stack_size].value->b.result = raised;
            gc_written(stack[stack_size].value);
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
