#include "eval.h"

#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Survey mode. Loads every module the program imports, directly or not, and
   prepares every definition in them (parse, resolve, infer) without running
   anything. Each failure is counted instead of ending the run. The report
   is the list of what is missing, most frequent first, and it is the
   measure of progress: definitions prepared out of all definitions. */

/* One kind of failure. */
typedef struct Failure {
    const char *message;
    const char *location; /* where it first happened, or NULL */
    int count;
} Failure;

static Failure *failures = NULL;
static int failure_count = 0;
static jmp_buf recover;

static void record_failure(const char *message, const char *location)
{
    int i;

    for (i = 0; i < failure_count; i++) {
        if (strcmp(failures[i].message, message) == 0) {
            failures[i].count++;
            return;
        }
    }
    failures = xrealloc(failures, (size_t)(failure_count + 1) * sizeof(Failure));
    failures[failure_count].message = copy_string(message, strlen(message));
    failures[failure_count].location =
        location != NULL ? copy_string(location, strlen(location)) : NULL;
    failures[failure_count].count = 1;
    failure_count++;
}

/* Called by die and todo instead of exiting. */
static void on_failure(const char *message, const char *location)
{
    record_failure(message, location);
    abandon_elaboration();
    longjmp(recover, 1);
}

/* Most frequent first; then by message, so that the order is stable. */
static int compare_failures(const void *a, const void *b)
{
    const Failure *x = a;
    const Failure *y = b;

    if (x->count != y->count) {
        return y->count - x->count;
    }
    return strcmp(x->message, y->message);
}

/* Prepares one definition. Returns whether that worked. */
static int prepare(Global *global)
{
    if (setjmp(recover) != 0) {
        return 0;
    }
    elaborate_global(global);
    return 1;
}

/* Whether the global has a definition of its own to prepare. A builtin and
   a class method without a default do not. */
static int has_definition(const Global *global)
{
    return global->builtin == NULL
        && (global->clauses != NULL || global->selector != NULL);
}

void run_survey(const char *file)
{
    Module *module;
    Entity *entity;
    Instance *instance;
    Global *global;
    Chunk *chunk;
    int modules = 0;
    int definitions = 0;
    int prepared = 0;
    int skipped = 0;
    int i;

    set_failure_handler(on_failure);

    /* Load everything. A module that cannot be loaded is a failure like any
       other. */
    if (setjmp(recover) == 0) {
        load_module_file(file);
    }
    for (;;) {
        if (setjmp(recover) != 0) {
            continue;
        }
        if (!load_next_import()) {
            break;
        }
    }

    for (module = first_loaded_module(); module != NULL; module = module->next) {
        modules++;
        for (chunk = module->unsupported; chunk != NULL; chunk = chunk->next) {
            /* Count the declarations the module loader skipped, by the
               word they start with. */
            char message[128];

            sprintf(message, "skipped declaration: %.80s",
                    module->tokens[chunk->start].text);
            record_failure(message, module->tokens[chunk->start].file);
            skipped++;
        }
        for (entity = module->entities; entity != NULL; entity = entity->next) {
            if (entity->namespace == NS_VARIABLE
                && has_definition(entity->global)) {
                definitions++;
                prepared += prepare(entity->global);
            }
        }
    }
    for (instance = all_instances; instance != NULL; instance = instance->next) {
        for (global = instance->methods; global != NULL; global = global->next) {
            definitions++;
            prepared += prepare(global);
        }
    }

    qsort(failures, (size_t)failure_count, sizeof(Failure), compare_failures);
    printf("modules loaded:        %d\n", modules);
    printf("definitions prepared:  %d of %d\n", prepared, definitions);
    printf("declarations skipped:  %d\n", skipped);
    printf("kinds of failure:      %d\n\n", failure_count);
    for (i = 0; i < failure_count; i++) {
        printf("%6d  %s", failures[i].count, failures[i].message);
        if (failures[i].location != NULL) {
            printf("  [%s]", failures[i].location);
        }
        printf("\n");
    }
}
