#include "eval.h"

#include <stdio.h>
#include <string.h>

/* Finding the definition of a class method at run time. The type the class
   is used at is matched against the head of every instance of the class. */

/* Whether the type has the shape of the instance head `pattern`. Fills in
   `bound` with the types of the instance's variables. */
static int match_type(Type *pattern, Type *type, const Instance *instance,
                      Type **bound)
{
    int i;

    pattern = prune(pattern);
    type = prune(type);
    switch (pattern->kind) {
    case TY_RIGID:
        for (i = 0; i < instance->variable_count; i++) {
            if (instance->variables[i] == pattern->variable) {
                bound[i] = type;
            }
        }
        return 1;
    case TY_CON:
        return type->kind == TY_CON
            && type->constructor == pattern->constructor;
    case TY_APP:
        return type->kind == TY_APP
            && match_type(pattern->function, type->function, instance, bound)
            && match_type(pattern->argument, type->argument, instance, bound);
    case TY_META:
        break;
    }
    return 0;
}

/* The first instance of the class that the type matches, or NULL.
   Overlapping instances are not handled: the first match wins. */
static Instance *find_instance(const Class *class, Type *type, Type ***bound)
{
    Instance *instance;
    int i;

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
        *bound = xmalloc((size_t)instance->variable_count * sizeof(Type *) + 1);
        for (i = 0; i < instance->variable_count; i++) {
            (*bound)[i] = NULL;
        }
        if (match_type(instance->head, type, instance, *bound)) {
            return instance;
        }
    }
    return NULL;
}

Value *find_method(Global *method, Type *type)
{
    Type **bound = NULL;
    Instance *instance = find_instance(method->class, type, &bound);
    Global *definition;
    Value *value;
    int i;

    if (instance == NULL) {
        /* The instance may be in a module that nothing has needed so far. */
        while (load_all_imports()) {
        }
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
