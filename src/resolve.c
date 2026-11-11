#include "syntax.h"

#include <string.h>

/* Name resolution. Every name is rewritten in place to point at what it
   refers to: a Binding, a Global or a Constructor. Once the operators of an
   E_OPERATORS are known, it is replaced by applications that follow their
   precedence. */

typedef struct Scope {
    Binding *binding;
    const struct Scope *outer;
} Scope;

static void resolve_in_scope(Module *module, const Scope *scope, Expr *expr);

static Binding *find_local(const Scope *scope, const char *name)
{
    for (; scope != NULL; scope = scope->outer) {
        if (strcmp(scope->binding->name, name) == 0) {
            return scope->binding;
        }
    }
    return NULL;
}

static const Scope *extend_scope(const Scope *outer, Binding *binding)
{
    Scope *scope = xmalloc(sizeof(Scope));

    scope->binding = binding;
    scope->outer = outer;
    return scope;
}

/* `name` is as written: Just, or Map.Bin with a qualifier. `prelude` is
   PRELUDE_NAME for a name that syntax stands for, else NULL. */
static Constructor *resolve_constructor(Module *module, const Token *token,
                                        const char *name, const char *prelude)
{
    const char *qualifier;
    const char *base = split_qualified(name, &qualifier);
    Entity *entity = lookup_entity(module, NS_CONSTRUCTOR,
                                   prelude != NULL ? prelude : qualifier, base);

    if (entity == NULL) {
        unresolved(module, token, name);
    }
    return entity->constructor;
}

/* The position of the named field in the constructor, or -1. */
static int field_index(const Constructor *constructor, const char *name)
{
    int i;

    for (i = 0; i < constructor->arity; i++) {
        if (constructor->field_names != NULL
            && constructor->field_names[i] != NULL
            && strcmp(constructor->field_names[i], name) == 0) {
            return i;
        }
    }
    return -1;
}

static Pattern *new_simple_pattern(enum PatternKind kind, const Token *token)
{
    Pattern *pattern = xmalloc(sizeof(Pattern));

    memset(pattern, 0, sizeof(Pattern));
    pattern->kind = kind;
    pattern->token = token;
    return pattern;
}

/* Rewrites C { f = p, g, .. } in place to the constructor pattern with one
   argument for each field of C: p for f, a variable g for g, and for the
   other fields a variable of the field's name if there is a `..`, else a
   wildcard. */
static void record_to_constructor_pattern(Module *module, Pattern *pattern)
{
    Constructor *constructor = resolve_constructor(
        module, pattern->token, pattern->name, pattern->qualifier);
    int i;
    int j;

    pattern->kind = P_CONSTRUCTOR;
    pattern->argument_count = constructor->arity;
    pattern->arguments =
        xmalloc((size_t)constructor->arity * sizeof(Pattern *) + 1);
    for (i = 0; i < constructor->arity; i++) {
        const char *name = constructor->field_names != NULL
            ? constructor->field_names[i] : NULL;
        Pattern *argument = NULL;
        int named = 0;

        for (j = 0; j < pattern->field_count; j++) {
            if (name != NULL && strcmp(pattern->field_names[j], name) == 0) {
                argument = pattern->field_patterns[j];
                named = 1;
            }
        }
        if (argument == NULL) {
            if (named || (pattern->field_wildcard && name != NULL)) {
                argument = new_simple_pattern(P_VARIABLE, pattern->token);
                argument->variable = new_binding(name);
            } else {
                argument = new_simple_pattern(P_WILDCARD, pattern->token);
            }
        }
        pattern->arguments[i] = argument;
    }
}

/* Resolves the constructors of a pattern and returns the scope extended
   with its variables. */
static const Scope *resolve_pattern(Module *module, const Scope *scope,
                                    Pattern *pattern)
{
    int i;

    if (pattern->kind == P_RECORD) {
        record_to_constructor_pattern(module, pattern);
    }
    switch (pattern->kind) {
    case P_VARIABLE:
        return extend_scope(scope, pattern->variable);
    case P_CONSTRUCTOR:
        pattern->constructor =
            resolve_constructor(module, pattern->token, pattern->name,
                                pattern->qualifier);
        for (i = 0; i < pattern->argument_count; i++) {
            scope = resolve_pattern(module, scope, pattern->arguments[i]);
        }
        return scope;
    case P_LITERAL:
        resolve_in_scope(module, scope, pattern->literal);
        break;
    case P_AS:
        return resolve_pattern(module, extend_scope(scope, pattern->variable),
                               pattern->inner);
    case P_BANG:
        return resolve_pattern(module, scope, pattern->inner);
    case P_WILDCARD:
    case P_CHAR:
    case P_RECORD:
        break;
    }
    return scope;
}

/* ---- Operator precedence ---- */

static const Fixity default_fixity = { "", ASSOC_LEFT, 9, NULL };
static const Fixity cons_fixity = { ":", ASSOC_RIGHT, 5, NULL };

/* The fixity of a resolved operator. */
static const Fixity *operator_fixity(const Expr *operator)
{
    const Fixity *fixity = NULL;

    if (operator->kind == E_GLOBAL) {
        fixity = find_fixity(operator->global->module, operator->global->name);
    } else if (operator->kind == E_CONSTRUCTOR) {
        if (operator->constructor->module == NULL) {
            return &cons_fixity;
        }
        fixity = find_fixity(operator->constructor->module,
                             operator->constructor->name);
    }
    return fixity != NULL ? fixity : &default_fixity;
}

static Expr *apply_operator(Expr *operator, Expr *left, Expr *right)
{
    Expr *inner = xmalloc(sizeof(Expr));
    Expr *outer = xmalloc(sizeof(Expr));

    memset(inner, 0, sizeof(Expr));
    inner->kind = E_APP;
    inner->token = left->token;
    inner->function = operator;
    inner->argument = left;
    *outer = *inner;
    outer->function = inner;
    outer->argument = right;
    return outer;
}

static Expr *build_operators(Module *module, const Expr *flat, int *index,
                             int minimum);

/* Precedence climbing. Extends `left`, which ends with operand `*index`,
   by the operators after it that have at least `minimum` precedence. On
   return `*index` is the operand the result ends with. */
static Expr *climb(Module *module, const Expr *flat, int *index, Expr *left,
                   int minimum)
{
    while (*index < flat->operator_count) {
        Expr *operator = flat->operators[*index];
        const Fixity *fixity = operator_fixity(operator);
        Expr *right;

        if (fixity->precedence < minimum) {
            break;
        }
        (*index)++;
        if (fixity->associativity == ASSOC_RIGHT) {
            right = build_operators(module, flat, index, fixity->precedence);
        } else {
            right = build_operators(module, flat, index, fixity->precedence + 1);
        }
        left = apply_operator(operator, left, right);
    }
    return left;
}

/* The expression that starts at operand `*index` and contains only
   operators of at least `minimum` precedence. A minus sign in front of an
   operand negates it together with the operators that bind tighter than
   subtraction: - a * b is negate (a * b), and - a + b is (negate a) + b. */
static Expr *build_operators(Module *module, const Expr *flat, int *index,
                             int minimum)
{
    Expr *left = flat->operands[*index];

    if (flat->negated[*index]) {
        Expr *negate = xmalloc(sizeof(Expr));
        Expr *application = xmalloc(sizeof(Expr));

        memset(negate, 0, sizeof(Expr));
        negate->kind = E_NAME;
        negate->token = left->token;
        negate->qualifier = PRELUDE_NAME;
        negate->name = "negate";
        resolve_in_scope(module, NULL, negate);
        memset(application, 0, sizeof(Expr));
        application->kind = E_APP;
        application->token = left->token;
        application->function = negate;
        application->argument = climb(module, flat, index, left, 7);
        left = application;
    }
    return climb(module, flat, index, left, minimum);
}

/* ---- Expressions ---- */

static Expr *new_simple_expr(enum ExprKind kind, const Token *token)
{
    Expr *expr = xmalloc(sizeof(Expr));

    memset(expr, 0, sizeof(Expr));
    expr->kind = kind;
    expr->token = token;
    return expr;
}

static Expr *new_simple_application(Expr *function, Expr *argument)
{
    Expr *expr = new_simple_expr(E_APP, function->token);

    expr->function = function;
    expr->argument = argument;
    return expr;
}

/* The value the record expression gives to the field, resolved, or NULL. A
   field given as { f } has the value of the variable f. */
static Expr *record_field_value(Module *module, const Scope *scope,
                                const Expr *record, const char *name)
{
    Expr *value = NULL;
    int j;

    for (j = 0; j < record->field_count; j++) {
        if (strcmp(record->field_names[j], name) == 0) {
            value = record->field_values[j];
            if (value == NULL) {
                value = new_simple_expr(E_NAME, record->token);
                value->name = name;
            }
        }
    }
    if (value != NULL) {
        resolve_in_scope(module, scope, value);
    }
    return value;
}

/* Rewrites C { f = a, g, .. } in place to C applied to one argument for each
   field of C: a for f, the variable g for g, and for the other fields the
   variable of the field's name if there is a `..`, else `undefined`. */
static void resolve_record_construction(Module *module, const Scope *scope,
                                        Expr *expr)
{
    Expr *result = expr->function;
    Constructor *constructor =
        resolve_constructor(module, result->token, result->name, NULL);
    int i;

    result->constructor = constructor;
    for (i = 0; i < constructor->arity; i++) {
        const char *name = constructor->field_names != NULL
            ? constructor->field_names[i] : NULL;
        Expr *value = name != NULL
            ? record_field_value(module, scope, expr, name) : NULL;

        if (value == NULL) {
            value = new_simple_expr(E_NAME, expr->token);
            if (expr->field_wildcard && name != NULL) {
                value->name = name;
            } else {
                value->name = "undefined";
                value->qualifier = PRELUDE_NAME;
            }
            resolve_in_scope(module, scope, value);
        }
        result = new_simple_application(result, value);
    }
    *expr = *result;
}

/* Rewrites e { f = a } in place to

       let u = a in case e of
         C v1 v2 v3 -> C v1 u v3      (for each constructor with the fields)

   The let keeps `a` in one place when there are several constructors. */
static void resolve_record_update(Module *module, const Scope *scope, Expr *expr)
{
    Entity *entity = lookup_entity(module, NS_VARIABLE, expr->field_qualifier,
                                   expr->field_names[0]);
    const TypeCon *type;
    Expr *let = new_simple_expr(E_LET, expr->token);
    Expr *match = new_simple_expr(E_CASE, expr->token);
    Alternative **last = &match->alternatives;
    int c;
    int i;
    int j;

    if (entity == NULL || entity->global->selector == NULL) {
        unresolved(module, expr->token, expr->field_names[0]);
    }
    type = entity->global->selector;
    resolve_in_scope(module, scope, expr->function);
    match->scrutinee = expr->function;

    let->body = match;
    let->binding_count = expr->field_count;
    let->bindings = xmalloc((size_t)expr->field_count * sizeof(Binding *));
    let->values = xmalloc((size_t)expr->field_count * sizeof(Expr *));
    for (j = 0; j < expr->field_count; j++) {
        let->bindings[j] = new_binding("");
        let->bindings[j]->definition = &let->values[j];
        let->values[j] =
            record_field_value(module, scope, expr, expr->field_names[j]);
    }

    for (c = 0; c < type->constructor_count; c++) {
        Constructor *constructor = type->constructors[c];
        Alternative *alternative;
        Pattern *pattern;
        Expr *body;
        int has_all = 1;

        for (j = 0; j < expr->field_count; j++) {
            if (field_index(constructor, expr->field_names[j]) < 0) {
                has_all = 0;
            }
        }
        if (!has_all) {
            continue;
        }
        pattern = new_simple_pattern(P_CONSTRUCTOR, expr->token);
        pattern->name = constructor->name;
        pattern->constructor = constructor;
        pattern->argument_count = constructor->arity;
        pattern->arguments =
            xmalloc((size_t)constructor->arity * sizeof(Pattern *) + 1);
        body = new_simple_expr(E_CONSTRUCTOR, expr->token);
        body->name = constructor->name;
        body->constructor = constructor;
        for (i = 0; i < constructor->arity; i++) {
            Expr *value = new_simple_expr(E_LOCAL, expr->token);

            pattern->arguments[i] = new_simple_pattern(P_VARIABLE, expr->token);
            pattern->arguments[i]->variable = new_binding("");
            value->local = pattern->arguments[i]->variable;
            for (j = 0; j < expr->field_count; j++) {
                if (field_index(constructor, expr->field_names[j]) == i) {
                    value->local = let->bindings[j];
                }
            }
            body = new_simple_application(body, value);
        }
        alternative = xmalloc(sizeof(Alternative));
        memset(alternative, 0, sizeof(Alternative));
        alternative->pattern = pattern;
        alternative->body = body;
        *last = alternative;
        last = &alternative->next;
    }
    if (match->alternatives == NULL) {
        unresolved(module, expr->token, expr->field_names[0]);
    }
    *expr = *let;
}

/* Resolves the definitions of an E_LET and returns the scope extended with
   its variables. */
static const Scope *resolve_bindings(Module *module, const Scope *scope,
                                     Expr *let)
{
    int i;

    for (i = 0; i < let->binding_count; i++) {
        scope = extend_scope(scope, let->bindings[i]);
    }
    for (i = 0; i < let->binding_count; i++) {
        resolve_in_scope(module, scope, let->values[i]);
    }
    return scope;
}

static void resolve_alternative(Module *module, const Scope *scope,
                                Alternative *alternative)
{
    Guard *guard;
    Qualifier *qualifier;

    scope = resolve_pattern(module, scope, alternative->pattern);
    if (alternative->where != NULL) {
        scope = resolve_bindings(module, scope, alternative->where);
    }
    if (alternative->body != NULL) {
        resolve_in_scope(module, scope, alternative->body);
    }
    for (guard = alternative->guards; guard != NULL; guard = guard->next) {
        const Scope *inner = scope;

        for (qualifier = guard->qualifiers; qualifier != NULL;
             qualifier = qualifier->next) {
            resolve_in_scope(module, inner, qualifier->expr);
            if (qualifier->pattern != NULL) {
                inner = resolve_pattern(module, inner, qualifier->pattern);
            }
        }
        resolve_in_scope(module, inner, guard->body);
    }
}

static void resolve_in_scope(Module *module, const Scope *scope, Expr *expr)
{
    Alternative *alternative;
    Entity *entity;
    int index = 0;
    int i;

    switch (expr->kind) {
    case E_NAME:
        if (expr->qualifier == NULL) {
            expr->local = find_local(scope, expr->name);
        }
        if (expr->local != NULL) {
            expr->kind = E_LOCAL;
            break;
        }
        entity = lookup_entity(module, NS_VARIABLE, expr->qualifier, expr->name);
        if (entity == NULL) {
            unresolved(module, expr->token, expr->name);
        }
        expr->global = entity->global;
        expr->kind = E_GLOBAL;
        break;
    case E_CONSTRUCTOR:
        expr->constructor =
            resolve_constructor(module, expr->token, expr->name, NULL);
        break;
    case E_APP:
        resolve_in_scope(module, scope, expr->function);
        resolve_in_scope(module, scope, expr->argument);
        break;
    case E_LAMBDA:
        resolve_in_scope(module, extend_scope(scope, expr->local), expr->body);
        break;
    case E_CASE:
        resolve_in_scope(module, scope, expr->scrutinee);
        for (alternative = expr->alternatives; alternative != NULL;
             alternative = alternative->next) {
            resolve_alternative(module, scope, alternative);
        }
        break;
    case E_OPERATORS:
        for (i = 0; i < expr->operator_count; i++) {
            resolve_in_scope(module, scope, expr->operators[i]);
        }
        for (i = 0; i <= expr->operator_count; i++) {
            resolve_in_scope(module, scope, expr->operands[i]);
        }
        *expr = *build_operators(module, expr, &index, 0);
        break;
    case E_LET:
        scope = resolve_bindings(module, scope, expr);
        resolve_in_scope(module, scope, expr->body);
        break;
    case E_RECORD:
        if (expr->function->kind == E_CONSTRUCTOR) {
            resolve_record_construction(module, scope, expr);
        } else {
            resolve_record_update(module, scope, expr);
        }
        break;
    case E_ANNOTATION:
        resolve_in_scope(module, scope, expr->body);
        break;
    case E_LOCAL:
    case E_GLOBAL:
    case E_INT:
    case E_CHAR:
    case E_STRING:
    case E_TYPE:
    case E_METHOD:
        break;
    }
}

void resolve(Module *module, Expr *expr)
{
    resolve_in_scope(module, NULL, expr);
}
