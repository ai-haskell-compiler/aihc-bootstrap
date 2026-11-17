#include "eval.h"

#include <stdio.h>
#include <stdlib.h>
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
    case TY_FORALL:
        break;
    }
    return 0;
}

static Type **new_bound(int count)
{
    Type **bound = gc_alloc((size_t)count * sizeof(Type *) + 1);
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

/* Adds the instances that have come since the last time to the class's
   own list, in front of it and in their order. */
static void collect_instances(Class *class)
{
    Instance *oldest = class->seen;
    Instance *older = class->instances;
    Instance **place = &class->instances;
    Instance *instance;

    class->seen = all_instances;
    for (instance = all_instances; instance != oldest; instance = instance->next) {
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
        instance->next_of_class = older;
        *place = instance;
        place = &instance->next_of_class;
    }
}

/* The first instance of the class that the type matches, or NULL.
   Overlapping instances are not handled: the first match wins. One case
   is: an instance for every type (instance C a) is only used if no other
   instance matches. */
static Instance *find_instance(Class *class, Type *type, Type ***bound)
{
    Instance *for_every_type = NULL;
    Instance *instance;

    /* Preparing an instance can load a module, with instances of its
       own. */
    while (class->seen != all_instances) {
        collect_instances(class);
    }
    for (instance = class->instances; instance != NULL;
         instance = instance->next_of_class) {
        if (prune(instance->head)->kind == TY_RIGID) {
            for_every_type = instance;
            continue;
        }
        *bound = new_bound(instance->variable_count);
        if (match_type(instance->head, type, instance->variable_count,
                       instance->variables, *bound)) {
            return instance;
        }
    }
    if (for_every_type != NULL) {
        *bound = new_bound(for_every_type->variable_count);
        (*bound)[0] = type;
    }
    return for_every_type;
}

/* ---- Remembering what was found ----

   A method is looked up every time it is called, and most calls are at a
   few types. So what find_method gives for a method at a type that has no
   variables in it is kept, and given again. New instances (from a module
   that was loaded) could change the answer, so what is kept is forgotten
   when there are any. The values are roots for the collector. */

typedef struct Remembered {
    Global *method;
    Type *type;
    Value *value;
    unsigned long hash;
    struct Remembered *next;
} Remembered;

#define REMEMBERED_BUCKETS 65536

static Remembered *remembered[REMEMBERED_BUCKETS];
static Instance *remembered_since = NULL; /* all_instances when it began */

/* A hash of the type, or 0 if the type has something in it that a later
   lookup could see differently: a variable or an associated type. */
static unsigned long hash_type(Type *type)
{
    unsigned long function;
    unsigned long argument;

    type = prune(type);
    switch (type->kind) {
    case TY_CON:
        if (type->constructor->is_family) {
            return 0;
        }
        return (unsigned long)(size_t)type->constructor / 8 * 2654435761UL + 1;
    case TY_APP:
        function = hash_type(type->function);
        argument = function == 0 ? 0 : hash_type(type->argument);
        if (argument == 0) {
            return 0;
        }
        return (function * 31 + argument) * 2654435761UL + 7;
    case TY_RIGID:
    case TY_META:
    case TY_FORALL:
        break;
    }
    return 0;
}

static void forget_if_instances_came(void)
{
    int i;

    if (remembered_since == all_instances) {
        return;
    }
    for (i = 0; i < REMEMBERED_BUCKETS; i++) {
        while (remembered[i] != NULL) {
            Remembered *next = remembered[i]->next;

            free(remembered[i]);
            remembered[i] = next;
        }
    }
    remembered_since = all_instances;
}

void mark_remembered_methods(void)
{
    int i;
    Remembered *entry;

    for (i = 0; i < REMEMBERED_BUCKETS; i++) {
        for (entry = remembered[i]; entry != NULL; entry = entry->next) {
            gc_mark(entry->value);
        }
    }
}

static Value *look_up_method(Global *method, Type *type);

Value *find_method(Global *method, Type *type)
{
    unsigned long hash = hash_type(type);
    Remembered *entry;
    Value *value;

    if (hash == 0) {
        return look_up_method(method, type);
    }
    hash = hash * 31 + (unsigned long)(size_t)method / 8;
    forget_if_instances_came();
    for (entry = remembered[hash % REMEMBERED_BUCKETS]; entry != NULL;
         entry = entry->next) {
        if (entry->hash == hash && entry->method == method
            && types_equal(entry->type, type)) {
            return entry->value;
        }
    }
    value = look_up_method(method, type);
    forget_if_instances_came();
    entry = xmalloc(sizeof(Remembered));
    entry->method = method;
    entry->type = type;
    entry->value = value;
    entry->hash = hash;
    entry->next = remembered[hash % REMEMBERED_BUCKETS];
    remembered[hash % REMEMBERED_BUCKETS] = entry;
    return value;
}

static Value *look_up_method(Global *method, Type *type)
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
