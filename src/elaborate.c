#include "eval.h"

#include <string.h>

/* The pipeline, run for one top-level variable the first time it is needed:

       tokens --parse--> Expr --resolve--> Expr --infer--> Expr

   parse    (parse.c)   also rewrites convenience syntax into core forms
   resolve  (resolve.c) names to what they refer to; operator precedence
   infer    (infer.c)   makes type arguments explicit */
static Expr *new_expr(enum ExprKind kind, const Token *token)
{
    Expr *expr = xmalloc(sizeof(Expr));

    memset(expr, 0, sizeof(Expr));
    expr->kind = kind;
    expr->token = token;
    return expr;
}

/* The definition of a record field used as a function:

       field = \record -> case record of
                            C _ x _ -> x     (for each constructor with it) */
static Expr *selector_definition(const Global *global)
{
    const TypeCon *type = global->selector;
    const Token *token = &global->module->tokens[0];
    Expr *lambda = new_expr(E_LAMBDA, token);
    Expr *match = new_expr(E_CASE, token);
    Alternative **last = &match->alternatives;
    int i;
    int j;
    int k;

    lambda->local = new_binding("");
    lambda->body = match;
    match->scrutinee = new_expr(E_LOCAL, token);
    match->scrutinee->local = lambda->local;
    for (i = 0; i < type->constructor_count; i++) {
        Constructor *constructor = type->constructors[i];

        for (k = 0; k < constructor->arity; k++) {
            if (constructor->field_names[k] != NULL
                && strcmp(constructor->field_names[k], global->name) == 0) {
                Alternative *alternative = xmalloc(sizeof(Alternative));
                Pattern *pattern = xmalloc(sizeof(Pattern));

                memset(alternative, 0, sizeof(Alternative));
                memset(pattern, 0, sizeof(Pattern));
                pattern->kind = P_CONSTRUCTOR;
                pattern->token = token;
                pattern->name = constructor->name;
                pattern->constructor = constructor;
                pattern->argument_count = constructor->arity;
                pattern->arguments =
                    xmalloc((size_t)constructor->arity * sizeof(Pattern *));
                for (j = 0; j < constructor->arity; j++) {
                    pattern->arguments[j] = xmalloc(sizeof(Pattern));
                    memset(pattern->arguments[j], 0, sizeof(Pattern));
                    pattern->arguments[j]->kind = P_WILDCARD;
                    pattern->arguments[j]->token = token;
                }
                pattern->arguments[k]->kind = P_VARIABLE;
                pattern->arguments[k]->variable = new_binding(global->name);
                alternative->pattern = pattern;
                alternative->body = new_expr(E_LOCAL, token);
                alternative->body->local = pattern->arguments[k]->variable;
                *last = alternative;
                last = &alternative->next;
            }
        }
    }
    return lambda;
}

/* The definitions being elaborated right now, outermost first. One can
   need another: a definition without a signature is elaborated when its type
   is first needed. */
static Global **elaborating = NULL;
static int elaborating_count = 0;
static int elaborating_capacity = 0;

void abandon_elaboration(void)
{
    int i;

    for (i = 0; i < elaborating_count; i++) {
        elaborating[i]->elaborating = 0;
    }
    if (elaborating_count > 0) {
        elaborating[elaborating_count - 1]->failed = 1;
    }
    elaborating_count = 0;
}

void elaborate_global(Global *global)
{
    Expr *expr;

    if (global->expr != NULL) {
        return;
    }
    if (global->failed) {
        TODO("definition that needs one that failed");
    }
    if (global->elaborating) {
        /* Its type is needed to work out its type. */
        TODO_AT(&global->module->tokens[global->clauses->start],
                "recursive definition without a type signature");
    }
    global->elaborating = 1;
    if (elaborating_count == elaborating_capacity) {
        elaborating_capacity = elaborating_capacity * 2 + 16;
        elaborating = xrealloc(elaborating, (size_t)elaborating_capacity
                                                * sizeof(Global *));
    }
    elaborating[elaborating_count++] = global;
    if (global->selector != NULL) {
        expr = selector_definition(global);
    } else if (global->synonym != NULL) {
        expr = parse_synonym_selector(global->synonym, global->name);
        resolve(global->module, expr);
    } else {
        expr = parse_global(global);
        resolve(global->module, expr);
    }
    global->expr = infer_global(global, expr);
    global->elaborating = 0;
    elaborating_count--;
}

Value *global_value(Global *global)
{
    if (global->value != NULL) {
        return global->value;
    }
    if (global->builtin != NULL) {
        const Builtin *builtin = find_builtin(global->builtin);

        /* A builtin without arguments is a plain value. */
        global->value = builtin->arity == 0 ? builtin->function(NULL)
                                            : make_builtin(builtin);
        return global->value;
    }
    elaborate_global(global);
    /* A thunk, so that the variable is evaluated at most once. */
    global->value = make_thunk(global->expr, NULL);
    return global->value;
}
