#include "eval.h"

#include <string.h>

/* usage: aihc-bootstrap [-X extension | -i directory]... Main.hs [argument]...

   Runs `main` of the given file. Imported modules are searched for in the
   -i directories. -X OverloadedStrings turns that extension on for the
   modules of the -i directories after it, and -X NoOverloadedStrings turns
   it off again; this is for the default-extensions of a .cabal file. The
   arguments after the file are the program's own.

   --survey runs nothing. It prepares every definition the program could
   reach and reports what the interpreter does not support (survey.c). */
int main(int argc, char **argv)
{
    const char *file = NULL;
    Module *module;
    Entity *entry;
    Entity *top;
    Value *action;
    int overloaded_strings = 0;
    int survey = 0;
    int i;

    for (i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-i") == 0 && i + 1 < argc) {
            add_search_directory(argv[++i], overloaded_strings);
        } else if (strcmp(argv[i], "--survey") == 0) {
            survey = 1;
        } else if (strcmp(argv[i], "-X") == 0 && i + 1 < argc) {
            i++;
            if (strcmp(argv[i], "OverloadedStrings") == 0) {
                overloaded_strings = 1;
            } else if (strcmp(argv[i], "NoOverloadedStrings") == 0) {
                overloaded_strings = 0;
            } else {
                die("-X %s is not supported", argv[i]);
            }
        } else {
            file = argv[i];
            set_program_arguments(argc - i - 1, argv + i + 1);
            break;
        }
    }
    if (file == NULL) {
        die("usage: aihc-bootstrap [-X extension | -i directory]... Main.hs [argument]...");
    }

    if (survey) {
        run_survey(file);
        return 0;
    }
    module = load_module_file(file);
    entry = find_entity(module, NS_VARIABLE, "main");
    if (entry == NULL) {
        die("%s does not define main", file);
    }
    /* runMain :: IO a -> IO () deals with exceptions that main does not
       catch. It takes the type a first, which nothing looks at. */
    top = find_entity(load_module("TopHandler"), NS_VARIABLE, "runMain");
    if (top == NULL) {
        die("TopHandler does not define runMain");
    }
    action = apply(apply(global_value(top->global), make_type(new_meta())),
                   global_value(entry->global));
    /* An IO action is a function from a world token; see eval.h. */
    apply(action, make_con(TAG_UNIT, 0));
    return 0;
}
