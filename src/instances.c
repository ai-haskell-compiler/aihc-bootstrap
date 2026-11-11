#include "eval.h"

#include <stdio.h>
#include <string.h>

/* Finding the definition of a class method at run time. The type the class
   is used at is matched against the head of every instance of the class. */

/* Whether the type has the shape of `pattern`, which is the head of an
   instance. Fills in `bound` with the types of the instance's variables. */
static int match_type(Type *pattern, Type *type, int variable_count,
                      Binding **variables, Type **bound)
{
    int i;

    pattern = prune(pattern);
    type = reduce_family(type);
    switch (pattern->kind) {
    case TY_RIGID:
        for (i = 0; i < variable_count; i++) {
            if (variables[i] == pattern->variable) {
                bound[i] = type;
            }
        }
        return 1;
    case TY_CON:
        return type->kind == TY_CON
            && type->constructor == pattern->constructor;
    case TY_APP:
        return type->kind == TY_APP
            && match_type(pattern->function, type->function, variable_count,
                          variables, bound)
            && match_type(pattern->argument, type->argument, variable_count,
                          variables, bound);
    case TY_META:
        break;
    }
    return 0;
}

static Type **new_bound(int count)
{
    Type **bound = xmalloc((size_t)count * sizeof(Type *) + 1);
    int i;

    for (i = 0; i < count; i++) {
        bound[i] = NULL;
    }
    return bound;
}

/* ---- Associated types ---- */

FamilyInstance *all_family_instances;

int is_family_application(Type *type)
{
    type = prune(type);
    if (type->kind != TY_APP) {
        return 0;
    }
    type = prune(type->function);
    return type->kind == TY_CON && type->constructor->is_family;
}

static void prepare_family_instance(FamilyInstance *instance)
{
    if (instance->lhs != NULL) {
        return;
    }
    instance->lhs = parse_type(instance->module, instance->lhs_start,
                               instance->lhs_end, &instance->variables,
                               &instance->variable_count, 1);
    instance->rhs = parse_type(instance->module, instance->rhs_start,
                               instance->rhs_end, &instance->variables,
                               &instance->variable_count, 1);
}

Type *reduce_family(Type *type)
{
    FamilyInstance *instance;
    const char *family;

    type = prune(type);
    if (!is_family_application(type)) {
        return type;
    }
    family = prune(type->function)->constructor->name;
    for (instance = all_family_instances; instance != NULL;
         instance = instance->next) {
        const Token *token = &instance->module->tokens[instance->lhs_start];
        const char *qualifier;
        Type **bound;

        /* Compare the names first, so that the instances of other
           associated types are never looked at more closely. */
        if (strcmp(split_qualified(token->text, &qualifier), family) != 0) {
            continue;
        }
        prepare_family_instance(instance);
        bound = new_bound(instance->variable_count);
        /* The left-hand side is the associated type applied to a type. */
        if (prune(instance->lhs->function)->constructor
                == prune(type->function)->constructor
            && match_type(instance->lhs->argument, type->argument,
                          instance->variable_count, instance->variables,
                          bound)) {
            return reduce_family(substitute(instance->rhs,
                                            instance->variable_count,
                                            instance->variables, bound));
        }
    }
    return type;
}

/* ---- Classes ---- */

/* The first instance of the class that the type matches, or NULL.
   Overlapping instances are not handled: the first match wins. */
static Instance *find_instance(const Class *class, Type *type, Type ***bound)
{
    Instance *instance;

    for (instance = all_instances; instance != NULL; instance = instance->next) {
        const Token *token = &instance->module->tokens[instance->class_token];
        const char *qualifier;

        /* Compare the names first, so that instances of other classes are
           never looked at more closely. */
        if (strcmp(split_qualified(token->text, &qualifier), class->name) != 0) {
            continue;
        }
        prepare_instance(instance);
        if (instance->class != class) {
            continue;
        }
        *bound = new_bound(instance->variable_count);
        if (match_type(instance->head, type, instance->variable_count,
                       instance->variables, *bound)) {
            return instance;
        }
    }
    return NULL;
}

Value *find_method(Global *method, Type *type)
{
    Type **bound = NULL;
    Instance *instance;
    Global *definition;
    Value *value;
    int i;

    type = reduce_family(type);
    instance = find_instance(method->class, type, &bound);
    if (instance == NULL) {
        /* The instance may be in a module that nothing has needed so far. */
        while (load_all_imports()) {
        }
        type = reduce_family(type);
        instance = find_instance(method->class, type, &bound);
    }
    if (instance == NULL) {
        fflush(stdout);
        fprintf(stderr, "no instance of %s for ", method->class->name);
        print_type(type);
        fprintf(stderr, "\n");
        die("needed for the method %s", method->name);
    }
    if (instance->derived_type != NULL) {
        value = derived_method(instance, method, type);
        if (value != NULL) {
            return value;
        }
    }
    for (definition = instance->methods; definition != NULL;
         definition = definition->next) {
        if (strcmp(definition->name, method->name) == 0) {
            /* The definition takes the instance's type variables first. */
            value = global_value(definition);
            for (i = 0; i < instance->variable_count; i++) {
                value = apply(value, make_type(bound[i] != NULL ? bound[i]
                                                                : new_meta()));
            }
            return value;
        }
    }
    /* The class's default definition takes the type the class is used at. */
    if (method->clauses == NULL) {
        die("an instance of %s defines no %s, and there is no default",
            method->class->name, method->name);
    }
    return apply(global_value(method), make_type(type));
}
