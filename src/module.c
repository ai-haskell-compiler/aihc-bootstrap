#include "syntax.h"

#include <ctype.h>
#include <stdio.h>
#include <string.h>

#define MAX_SEARCH_DIRECTORIES 64

static const char *search_directories[MAX_SEARCH_DIRECTORIES];
static int search_directory_overloaded_strings[MAX_SEARCH_DIRECTORIES];
static int search_directory_count = 0;
static Module *loaded_modules = NULL;

Instance *all_instances = NULL;

const char *const PRELUDE_NAME = "Prelude";

/* The types and constructors that have their own syntax. */
TypeCon function_type_constructor = { "->", NULL, 0, NULL, 0, 0, 0, NULL, 0, NULL, 0 };
TypeCon list_type_constructor = { "[]", NULL, 0, NULL, 0, 0, 0, NULL, 0, NULL, 0 };
TypeCon unit_type_constructor = { "()", NULL, 0, NULL, 0, 0, 0, NULL, 0, NULL, 0 };
Constructor unit_constructor =
    { "()", &unit_type_constructor, NULL, 0, 0, 0, 0, NULL, NULL, NULL, NULL, NULL, 0, NULL };
Constructor nil_constructor =
    { "[]", &list_type_constructor, NULL, 0, 0, 0, 0, NULL, NULL, NULL, NULL, NULL, 0, NULL };
Constructor cons_constructor =
    { ":", &list_type_constructor, NULL, 1, 2, 0, 0, NULL, NULL, NULL, NULL, NULL, 0, NULL };

static Entity unit_entity =
    { NS_CONSTRUCTOR, "()", "()", NULL, &unit_constructor, NULL, NULL, NULL };
static Entity nil_entity =
    { NS_CONSTRUCTOR, "[]", "[]", NULL, &nil_constructor, NULL, NULL, NULL };
static Entity cons_entity =
    { NS_CONSTRUCTOR, ":", "[]", NULL, &cons_constructor, NULL, NULL, NULL };

static TypeCon tuple_types[MAX_TUPLE + 1];
static Constructor tuple_constructors[MAX_TUPLE + 1];
static Entity tuple_entities[MAX_TUPLE + 1];

const char *tuple_name(int arity)
{
    static char names[MAX_TUPLE + 1][MAX_TUPLE + 2];

    if (arity < 2 || arity > MAX_TUPLE) {
        TODO("tuple with more elements than MAX_TUPLE");
    }
    /* "(" then arity - 1 commas then ")" */
    if (names[arity][0] == '\0') {
        names[arity][0] = '(';
        memset(names[arity] + 1, ',', (size_t)arity - 1);
        names[arity][arity] = ')';
        names[arity][arity + 1] = '\0';
    }
    return names[arity];
}

/* The tuple type, constructor and entity of that arity, made on first use. */
static Entity *tuple_entity(int arity)
{
    TypeCon *type = &tuple_types[arity];
    Constructor *constructor = &tuple_constructors[arity];
    Entity *entity = &tuple_entities[arity];
    int i;

    if (type->name == NULL) {
        type->name = tuple_name(arity);
        type->parameter_count = arity;
        type->parameters = xmalloc((size_t)arity * sizeof(Binding *));
        for (i = 0; i < arity; i++) {
            type->parameters[i] = new_binding("t");
        }
        constructor->name = type->name;
        constructor->type = type;
        constructor->arity = arity;
        entity->namespace = NS_CONSTRUCTOR;
        entity->name = type->name;
        entity->parent = type->name;
        entity->constructor = constructor;
    }
    return entity;
}

TypeCon *tuple_type_constructor(int arity)
{
    return tuple_entity(arity)->constructor->type;
}

void add_search_directory(const char *directory, int overloaded_strings)
{
    if (search_directory_count == MAX_SEARCH_DIRECTORIES) {
        die("too many search directories");
    }
    search_directories[search_directory_count] = directory;
    search_directory_overloaded_strings[search_directory_count] =
        overloaded_strings;
    search_directory_count++;
}

Entity *library_entity(const char *module, enum Namespace namespace,
                       const char *name)
{
    Entity *entity =
        find_exported_entity(load_module(module), namespace, name);

    if (entity == NULL) {
        die("the library module %s does not define %s", module, name);
    }
    return entity;
}

/* ---- Looking up names ---- */

Entity *find_entity(Module *module, enum Namespace namespace, const char *name)
{
    Entity *entity;

    for (entity = module->entities; entity != NULL; entity = entity->next) {
        if (entity->namespace == namespace && strcmp(entity->name, name) == 0) {
            return entity;
        }
    }
    return NULL;
}

static Module *imported_module(Import *import);
static int import_allows(const Import *import, const char *name,
                         const char *parent);
static int import_might_provide(const Import *import,
                                enum Namespace namespace, const char *name,
                                int associated_types_too);

/* A name the module imports and lists among its exports, as itself or as
   one of the children of a T (..). Only the imports that could have it are
   looked at, so that no module is loaded for nothing. */
static Entity *find_exported_within(Module *module, enum Namespace namespace,
                                    const char *name);

static Entity *find_listed_export(Module *module, enum Namespace namespace,
                                  const char *name)
{
    const ImportItem *item;
    Import *import;

    for (item = module->exports->items; item != NULL; item = item->next) {
        int by_name = strcmp(item->name, name) == 0;

        if (!by_name && !(item->all_children && namespace != NS_TYPE
                          && find_entity(module, NS_TYPE, item->name) == NULL)) {
            continue;
        }
        for (import = module->imports; import != NULL; import = import->next) {
            Entity *found;

            /* For T (..), the import has to be one that T can come from. */
            if (!import_might_provide(import, by_name ? namespace : NS_TYPE,
                                      item->name, 0)) {
                continue;
            }
            found = find_exported_within(imported_module(import), namespace,
                                         name);
            if (found != NULL && import_allows(import, name, found->parent)
                && import_allows(module->exports, name, found->parent)) {
                return found;
            }
        }
    }
    return NULL;
}

/* Each search of exports has a number. A module remembers the last search
   that came to it, so that a search asks every module only once: modules
   re-export each other in circles and along many paths. */
static unsigned long current_search = 0;

static Entity *find_exported_within(Module *module, enum Namespace namespace,
                                    const char *name)
{
    Entity *entity = find_entity(module, namespace, name);
    Import *import;
    int i;

    if (entity != NULL || module->searched == current_search) {
        return entity;
    }
    module->searched = current_search;
    for (i = 0; i < module->reexport_count && entity == NULL; i++) {
        /* The name in the export list is that of an import, or its alias. */
        for (import = module->imports; import != NULL && entity == NULL;
             import = import->next) {
            if (strcmp(import->alias, module->reexports[i]) == 0
                || strcmp(import->name, module->reexports[i]) == 0) {
                entity = find_exported_within(imported_module(import),
                                              namespace, name);
            }
        }
    }
    if (entity == NULL && module->exports != NULL) {
        entity = find_listed_export(module, namespace, name);
    }
    return entity;
}

Entity *find_exported_entity(Module *module, enum Namespace namespace,
                             const char *name)
{
    current_search++;
    return find_exported_within(module, namespace, name);
}

const Fixity *find_fixity(const Module *module, const char *name)
{
    const Fixity *fixity;

    for (fixity = module->fixities; fixity != NULL; fixity = fixity->next) {
        if (strcmp(fixity->name, name) == 0) {
            return fixity;
        }
    }
    return NULL;
}

/* Whether the import can be used to refer to a name written with the given
   qualifier (NULL for an unqualified name). */
static int import_matches_qualifier(const Import *import, const char *qualifier)
{
    if (qualifier == NULL) {
        return !import->qualified;
    }
    return strcmp(import->alias, qualifier) == 0;
}

/* Whether the import list lets the name through. `parent` is what T (..)
   would have to name to include it, or NULL.

   The import list does not say which kind of name it means: `import M (T)`
   also lets a constructor called T through. */
static int import_allows(const Import *import, const char *name,
                         const char *parent)
{
    const ImportItem *item;
    int listed = 0;

    if (!import->has_list) {
        return 1;
    }
    for (item = import->items; item != NULL; item = item->next) {
        if (strcmp(item->name, name) == 0) {
            listed = 1;
        }
        if (parent != NULL && item->all_children
            && strcmp(item->name, parent) == 0) {
            listed = 1;
        }
    }
    return import->hiding ? !listed : listed;
}

/* A cheap test that needs no knowledge of the imported module. It lets us
   leave a module unloaded when its import list rules the name out. */
static int import_might_provide(const Import *import,
                                enum Namespace namespace, const char *name,
                                int associated_types_too)
{
    const ImportItem *item;

    if (!import->has_list || import->hiding) {
        return 1;
    }
    for (item = import->items; item != NULL; item = item->next) {
        /* T (..) can stand for any constructor or variable. It can also
           stand for a type, if T is a class with associated types; that is
           rare, so it is only considered when asked for. */
        if ((item->all_children
             && (namespace != NS_TYPE || associated_types_too))
            || strcmp(item->name, name) == 0) {
            return 1;
        }
    }
    return 0;
}

static Module *imported_module(Import *import)
{
    if (import->module == NULL) {
        import->module = load_module(import->name);
    }
    return import->module;
}

/* What the name means in the module: its own definition, or one it
   imports. With `associated_types_too`, an import of C (..) is also looked
   at for a type name. */
static Entity *lookup(Module *module, enum Namespace namespace,
                      const char *qualifier, const char *name,
                      int associated_types_too)
{
    Entity *found = NULL;
    Import *import;

    if (qualifier == PRELUDE_NAME) {
        return find_exported_entity(load_module("Prelude"), namespace, name);
    }
    if (qualifier == NULL) {
        if (namespace == NS_CONSTRUCTOR && strcmp(name, "()") == 0) {
            return &unit_entity;
        }
        if (namespace == NS_CONSTRUCTOR && strcmp(name, "[]") == 0) {
            return &nil_entity;
        }
        if (namespace == NS_CONSTRUCTOR && strcmp(name, ":") == 0) {
            return &cons_entity;
        }
        if (namespace == NS_CONSTRUCTOR && name[0] == '(' && name[1] == ',') {
            return tuple_entity((int)strlen(name) - 1);
        }
        found = find_entity(module, namespace, name);
        if (found != NULL) {
            return found;
        }
    }
    for (import = module->imports; import != NULL; import = import->next) {
        Entity *entity;

        if (!import_matches_qualifier(import, qualifier)
            || !import_might_provide(import, namespace, name,
                                     associated_types_too)) {
            continue;
        }
        entity = find_exported_entity(imported_module(import), namespace, name);
        if (entity == NULL || !import_allows(import, name, entity->parent)) {
            continue;
        }
        if (found != NULL && entity != found) {
            die("%s: ambiguous name %s", module->name, name);
        }
        found = entity;
    }
    if (found == NULL && namespace == NS_TYPE && !associated_types_too) {
        /* Perhaps an associated type, imported with its class as C (..). */
        return lookup(module, namespace, qualifier, name, 1);
    }
    return found;
}

Entity *lookup_entity(Module *module, enum Namespace namespace,
                      const char *qualifier, const char *name)
{
    return lookup(module, namespace, qualifier, name, 0);
}

const char *split_qualified(const char *name, const char **qualifier)
{
    const char *dot = strrchr(name, '.');

    *qualifier = NULL;
    if (dot == NULL || !isupper((unsigned char)name[0])) {
        return name;
    }
    *qualifier = copy_string(name, (size_t)(dot - name));
    return dot + 1;
}

void note_unsupported(const Module *module)
{
    const Chunk *chunk;
    const Token *first;
    int count = 0;

    for (chunk = module->unsupported; chunk != NULL; chunk = chunk->next) {
        count++;
    }
    if (count == 0) {
        return;
    }
    /* The list is built by prepending, so the first declaration is last. */
    for (chunk = module->unsupported; chunk->next != NULL; chunk = chunk->next) {
    }
    first = &module->tokens[chunk->start];
    fprintf(stderr, "note: %s skips %d declaration(s); the first is '%s' at %s:%d\n",
            module->name, count, first->text, first->file, first->line);
}

void unresolved(Module *module, const Token *token, const char *name)
{
    const Import *import;
    char *message = xmalloc(strlen(name) + 32);

    if (!has_failure_handler()) {
        fflush(stdout);
        note_unsupported(module);
        for (import = module->imports; import != NULL; import = import->next) {
            if (import->module != NULL) {
                note_unsupported(import->module);
            }
        }
    }
    strcpy(message, "unresolved name: ");
    strcat(message, name);
    TODO_AT(token, message);
}

/* ---- Small helpers for looking at tokens ---- */

static int is_open_bracket(const Token *token)
{
    return token->kind == T_SPECIAL
        && (token->text[0] == '(' || token->text[0] == '[');
}

static int is_close_bracket(const Token *token)
{
    return token->kind == T_SPECIAL
        && (token->text[0] == ')' || token->text[0] == ']');
}

/* The first token in [start, end) that is the given reserved word and not
   inside brackets, or `end`. */
static int find_reserved(const Token *tokens, int start, int end,
                         const char *text)
{
    int depth = 0;
    int i;

    for (i = start; i < end; i++) {
        if (is_open_bracket(&tokens[i])) {
            depth++;
        } else if (is_close_bracket(&tokens[i])) {
            depth--;
        } else if (depth == 0 && token_is(&tokens[i], T_RESERVED, text)) {
            return i;
        }
    }
    return end;
}

/* The end of the layout item that starts at `start`: the next token that is
   first on its line and in the given column. */
static int item_end(const Token *tokens, int start, int end, int column)
{
    int i = start + 1;

    while (i < end && !(tokens[i].col == column
                        && tokens[i].line != tokens[i - 1].line)) {
        i++;
    }
    return i;
}

/* If [start, end) is a type signature `name, (+) :: type`, the position of
   its `::`. Otherwise -1. */
static int signature_colon(const Token *tokens, int start, int end)
{
    int i = start;

    for (;;) {
        if (i < end && tokens[i].kind == T_VARID) {
            i++;
        } else if (i + 2 < end && token_is(&tokens[i], T_SPECIAL, "(")
                   && tokens[i + 1].kind == T_VARSYM
                   && token_is(&tokens[i + 2], T_SPECIAL, ")")) {
            i += 3;
        } else {
            return -1;
        }
        if (i >= end) {
            return -1;
        }
        if (token_is(&tokens[i], T_RESERVED, "::")) {
            return i;
        }
        if (!token_is(&tokens[i], T_SPECIAL, ",")) {
            return -1;
        }
        i++;
    }
}

/* If [start, end) is a clause `name parameters = body`,
   `(+) parameters = body` or `parameter + parameter = body`, the name.
   Otherwise NULL. */
static const char *clause_name(const Token *tokens, int start, int end)
{
    const Token *after; /* the token after the name */
    const char *name;

    if (tokens[start].kind == T_VARID) {
        name = tokens[start].text;
        after = &tokens[start + 1];
    } else if (end - start > 3 && token_is(&tokens[start], T_SPECIAL, "(")
               && tokens[start + 1].kind == T_VARSYM
               && token_is(&tokens[start + 2], T_SPECIAL, ")")) {
        name = tokens[start + 1].text;
        after = &tokens[start + 3];
    } else {
        after = NULL;
    }
    if (after == NULL
        || (after->kind == T_VARSYM && !token_is_bang(after - 1, after))
        || token_is(after, T_SPECIAL, "`")) {
        /* Perhaps an operator defined between its parameters:
           x + y = ... */
        int operator = infix_clause_operator(tokens, start, end);

        if (operator < 0) {
            return NULL;
        }
        return tokens[operator].kind == T_VARSYM ? tokens[operator].text
                                                 : tokens[operator + 1].text;
    }
    return name;
}

/* ---- Recording declarations ---- */

static Entity *add_entity(Module *module, enum Namespace namespace,
                          const char *name, const char *parent)
{
    Entity *entity = xmalloc(sizeof(Entity));

    memset(entity, 0, sizeof(Entity));
    entity->namespace = namespace;
    entity->name = name;
    entity->parent = parent;
    entity->next = module->entities;
    module->entities = entity;
    return entity;
}

static Global *new_global(Module *module, const char *name)
{
    Global *global = xmalloc(sizeof(Global));

    memset(global, 0, sizeof(Global));
    global->name = name;
    global->module = module;
    return global;
}

/* The top-level variable or class method of that name. */
static Entity *find_or_add_variable(Module *module, const char *name)
{
    Entity *entity = find_entity(module, NS_VARIABLE, name);

    if (entity == NULL) {
        entity = add_entity(module, NS_VARIABLE, name, NULL);
        entity->global = new_global(module, name);
    }
    return entity;
}

static Chunk *new_chunk(int start, int end)
{
    Chunk *chunk = xmalloc(sizeof(Chunk));

    chunk->start = start;
    chunk->end = end;
    chunk->next = NULL;
    return chunk;
}

static void add_clause(Global *global, int start, int end)
{
    Chunk **last = &global->clauses;

    while (*last != NULL) {
        last = &(*last)->next;
    }
    *last = new_chunk(start, end);
}

static void add_unsupported(Module *module, int start, int end)
{
    Chunk *chunk = new_chunk(start, end);

    chunk->next = module->unsupported;
    module->unsupported = chunk;
}

static Import *add_import(Module *module, const char *name)
{
    Import *import = xmalloc(sizeof(Import));

    memset(import, 0, sizeof(Import));
    import->name = name;
    import->alias = name;
    import->next = module->imports;
    module->imports = import;
    return import;
}

static ImportItem *add_import_item(Import *import, const char *name)
{
    ImportItem *item = xmalloc(sizeof(ImportItem));

    item->name = name;
    item->all_children = 0;
    item->next = import->items;
    import->items = item;
    return item;
}

/* The list of an import: (foldr, (+), Maybe (..), Either (Left, Right)).
   `start` is the opening parenthesis. The names inside T (...) are added as
   items of their own. */
static void add_import_items(Import *import, const Token *tokens, int start,
                             int end)
{
    ImportItem *parent_item = NULL; /* the T of the T (...) we are next to */
    int in_sublist = 0;
    int i;

    for (i = start + 1; i < end; i++) {
        const Token *token = &tokens[i];

        if (token_is(token, T_SPECIAL, "(")) {
            if (i + 2 < end && token_is(&tokens[i + 2], T_SPECIAL, ")")
                && (tokens[i + 1].kind == T_VARSYM
                    || token_is(&tokens[i + 1], T_RESERVED, ":"))) {
                add_import_item(import, tokens[i + 1].text); /* (+) */
                i += 2;
            } else {
                in_sublist = 1;
            }
        } else if (token_is(token, T_SPECIAL, ")")) {
            in_sublist = 0;
        } else if (token_is(token, T_RESERVED, "..")) {
            if (in_sublist && parent_item != NULL) {
                parent_item->all_children = 1;
            }
        } else if (token_is(token, T_VARID, "pattern")
                   && tokens[i + 1].kind == T_CONID) {
            /* `pattern P`: the keyword adds nothing. */
        } else if (token->kind == T_VARID) {
            add_import_item(import, token->text);
        } else if (token->kind == T_CONID) {
            ImportItem *item = add_import_item(import, token->text);

            if (!in_sublist) {
                parent_item = item;
            }
        }
    }
}

/* import [qualified] Name [qualified] [as Alias] [hiding] [(items)] */
static void add_import_declaration(Module *module, int start, int end)
{
    const Token *tokens = module->tokens;
    Import *import;
    int qualified = 0;
    int i = start + 1;

    if (i < end && token_is(&tokens[i], T_VARID, "qualified")) {
        qualified = 1;
        i++;
    }
    if (i == end || tokens[i].kind != T_CONID) {
        TODO_AT(&tokens[start], "import that does not name a module directly");
    }
    import = add_import(module, tokens[i].text);
    i++;
    if (i < end && token_is(&tokens[i], T_VARID, "qualified")) {
        qualified = 1;
        i++;
    }
    import->qualified = qualified;
    if (i + 1 < end && token_is(&tokens[i], T_VARID, "as")) {
        import->alias = tokens[i + 1].text;
        i += 2;
    }
    if (i < end && token_is(&tokens[i], T_VARID, "hiding")) {
        import->hiding = 1;
        i++;
    }
    if (i < end) {
        import->has_list = 1;
        add_import_items(import, tokens, i, end);
    }
}

/* foreign import prim "name" variable :: type */
static void add_foreign_declaration(Module *module, int start, int end)
{
    const Token *tokens = module->tokens;
    Global *global;

    if (end - start < 6
        || !token_is(&tokens[start + 1], T_RESERVED, "import")
        || !token_is(&tokens[start + 2], T_VARID, "prim")
        || tokens[start + 3].kind != T_STRING
        || tokens[start + 4].kind != T_VARID
        || !token_is(&tokens[start + 5], T_RESERVED, "::")) {
        TODO_AT(&tokens[start], "foreign declaration other than import prim");
    }
    global = find_or_add_variable(module, tokens[start + 4].text)->global;
    global->builtin = tokens[start + 3].text;
    global->signature_start = start + 6;
    global->signature_end = end;
}

/* infixl 6 +, - */
static void add_fixity_declaration(Module *module, int start, int end)
{
    const Token *tokens = module->tokens;
    enum Associativity associativity = ASSOC_NONE;
    int precedence = 9;
    int i = start + 1;

    if (strcmp(tokens[start].text, "infixl") == 0) {
        associativity = ASSOC_LEFT;
    } else if (strcmp(tokens[start].text, "infixr") == 0) {
        associativity = ASSOC_RIGHT;
    }
    if (i < end && tokens[i].kind == T_INT) {
        precedence = (int)tokens[i].number;
        i++;
    }
    for (; i < end; i++) {
        /* Everything except the commas and backquotes is an operator name. */
        if (tokens[i].kind != T_SPECIAL) {
            Fixity *fixity = xmalloc(sizeof(Fixity));

            fixity->name = tokens[i].text;
            fixity->associativity = associativity;
            fixity->precedence = precedence;
            fixity->next = module->fixities;
            module->fixities = fixity;
        }
    }
}

static int is_fixity_keyword(const Token *token)
{
    return token_is(token, T_RESERVED, "infixl")
        || token_is(token, T_RESERVED, "infixr")
        || token_is(token, T_RESERVED, "infix");
}

/* The `T a b` of a data, newtype or type declaration, starting at
   `*position`. Returns NULL if that is not what is there. */
static TypeCon *add_type_head(Module *module, int *position, int end)
{
    const Token *tokens = module->tokens;
    TypeCon *type;
    int i = *position;

    if (i == end || tokens[i].kind != T_CONID) {
        return NULL;
    }
    type = xmalloc(sizeof(TypeCon));
    memset(type, 0, sizeof(TypeCon));
    type->name = tokens[i].text;
    type->module = module;
    for (i++; i < end && tokens[i].kind == T_VARID; i++) {
        type->parameters = xrealloc(
            type->parameters,
            (size_t)(type->parameter_count + 1) * sizeof(Binding *));
        type->parameters[type->parameter_count++] =
            new_binding(tokens[i].text);
    }
    add_entity(module, NS_TYPE, type->name, NULL)->type = type;
    *position = i;
    return type;
}

static void add_field(Constructor *constructor, const char *name, int start,
                      int end, int strict)
{
    size_t count = (size_t)constructor->arity + 1;

    constructor->field_starts =
        xrealloc(constructor->field_starts, count * sizeof(int));
    constructor->field_ends =
        xrealloc(constructor->field_ends, count * sizeof(int));
    constructor->field_strict =
        xrealloc(constructor->field_strict, count * sizeof(int));
    constructor->field_names =
        xrealloc(constructor->field_names, count * sizeof(const char *));
    constructor->field_starts[constructor->arity] = start;
    constructor->field_ends[constructor->arity] = end;
    constructor->field_strict[constructor->arity] = strict;
    constructor->field_names[constructor->arity] = name;
    constructor->arity++;
}

/* The end of the field type that starts at `start`: one token, or a group
   in brackets. */
static int field_end(const Token *tokens, int start, int end)
{
    int depth = 0;
    int i;

    for (i = start; i < end; i++) {
        if (is_open_bracket(&tokens[i])) {
            depth++;
        } else if (is_close_bracket(&tokens[i])) {
            depth--;
        }
        if (depth == 0) {
            return i + 1;
        }
    }
    return end;
}

/* A constructor written between its two fields: a :| [a]. Returns NULL if
   that is not what is at `*position`. */
static Constructor *read_infix_constructor(Module *module,
                                           Constructor *constructor,
                                           int *position, int end)
{
    const Token *tokens = module->tokens;
    int i = *position;
    int side;

    (void)module;
    for (side = 0; side < 2; side++) {
        int strict = 0;
        int stop;

        if (i < end && token_is(&tokens[i], T_VARSYM, "!")) {
            strict = 1;
            i++;
        }
        if (i >= end || (!is_open_bracket(&tokens[i]) && tokens[i].kind != T_VARID
                         && tokens[i].kind != T_CONID)) {
            return NULL;
        }
        stop = field_end(tokens, i, end);
        add_field(constructor, NULL, i, stop, strict);
        i = stop;
        if (side == 0) {
            if (i >= end || tokens[i].kind != T_VARSYM || tokens[i].text[0] != ':') {
                return NULL;
            }
            constructor->name = tokens[i].text;
            constructor->is_infix = 1;
            i++;
        }
    }
    *position = i;
    return constructor;
}

/* One constructor of a data declaration, starting at `*position`:

       [forall e. [context =>]] Name field types
       [forall e. [context =>]] Name { field names and types }
       [forall e. [context =>]] field type :Name field type

   Records its name and the tokens of each field type. Returns NULL for
   forms that are not supported yet. */
static Constructor *read_constructor(Module *module, int *position, int end)
{
    const Token *tokens = module->tokens;
    Constructor *constructor;
    int depth = 0;
    int i = *position;

    constructor = xmalloc(sizeof(Constructor));
    memset(constructor, 0, sizeof(Constructor));
    constructor->module = module;
    if (i < end && token_is(&tokens[i], T_VARID, "forall")) {
        int limit;
        int arrow;

        for (i++; i < end && tokens[i].kind == T_VARID; i++) {
            constructor->existentials = xrealloc(
                constructor->existentials,
                (size_t)(constructor->existential_count + 1) * sizeof(Binding *));
            constructor->existentials[constructor->existential_count++] =
                new_binding(tokens[i].text);
        }
        if (i == end || !token_is(&tokens[i], T_VARSYM, ".")) {
            return NULL;
        }
        i++;
        /* The context is not needed: see Instance in syntax.h. */
        limit = find_reserved(tokens, i, end, "|");
        arrow = find_reserved(tokens, i, limit, "=>");
        if (arrow < limit) {
            i = arrow + 1;
        }
    }
    if (i < end && tokens[i].kind != T_CONID) {
        return read_infix_constructor(module, constructor, position, end);
    }
    if (i == end) {
        return NULL;
    }
    constructor->name = tokens[i].text;
    i++;
    if (i < end && token_is(&tokens[i], T_SPECIAL, "{")) {
        /* A record: { name, name :: type, name :: type } */
        i++;
        while (i < end && !token_is(&tokens[i], T_SPECIAL, "}")) {
            int names = i;
            int colon = find_reserved(tokens, i, end, "::");
            int type_start = colon + 1;
            int strict = 0;

            if (colon == end) {
                return NULL;
            }
            if (token_is(&tokens[type_start], T_VARSYM, "!")) {
                strict = 1;
                type_start++;
            }
            /* The type ends at the comma or the closing brace. */
            for (i = type_start; i < end; i++) {
                if (depth == 0 && (token_is(&tokens[i], T_SPECIAL, ",")
                                   || token_is(&tokens[i], T_SPECIAL, "}"))) {
                    break;
                }
                if (is_open_bracket(&tokens[i])) {
                    depth++;
                } else if (is_close_bracket(&tokens[i])) {
                    depth--;
                }
            }
            for (; names < colon; names++) {
                if (tokens[names].kind == T_VARID) {
                    add_field(constructor, tokens[names].text, type_start, i,
                              strict);
                }
            }
            if (i < end && token_is(&tokens[i], T_SPECIAL, ",")) {
                i++;
            }
        }
        i++; /* the closing brace */
    } else {
        /* Fields without names. Each is a name or a group in brackets, and
           may have a ! in front. */
        int strict = 0;
        int start = i;

        for (; i < end; i++) {
            const Token *token = &tokens[i];

            if (depth == 0 && (token_is(token, T_RESERVED, "|")
                               || token_is(token, T_RESERVED, "deriving"))) {
                break;
            }
            if (depth == 0 && token_is(token, T_VARSYM, "!")) {
                strict = 1;
                continue;
            }
            if (depth == 0) {
                if (!is_open_bracket(token) && token->kind != T_VARID
                    && token->kind != T_CONID) {
                    return NULL;
                }
                start = i;
            }
            if (is_open_bracket(token)) {
                depth++;
            } else if (is_close_bracket(token)) {
                depth--;
            }
            if (depth == 0) {
                add_field(constructor, NULL, start, i + 1, strict);
                strict = 0;
            }
        }
    }
    *position = i;
    return constructor;
}

static void add_derived_instance(Module *module, TypeCon *type, int class_token,
                                 int strategy)
{
    Instance *instance = xmalloc(sizeof(Instance));

    memset(instance, 0, sizeof(Instance));
    instance->module = module;
    instance->class_token = class_token;
    instance->derived_type = type;
    instance->derived_strategy = strategy;
    instance->next = all_instances;
    all_instances = instance;
}

/* The deriving clauses of a data declaration, starting at `start`:

       deriving [stock | newtype | anyclass] (C, D) [via T]
       deriving C

   Each class becomes an instance without definitions (see derive.c). */
static void add_deriving_clauses(Module *module, TypeCon *type, int start,
                                 int end)
{
    const Token *tokens = module->tokens;
    int i = start;

    while (i < end && token_is(&tokens[i], T_RESERVED, "deriving")) {
        int strategy = DERIVE_DEFAULT;
        int clause_end = find_reserved(tokens, i + 1, end, "deriving");
        int depth = 0;
        int j;

        i++;
        if (i < clause_end && token_is(&tokens[i], T_VARID, "stock")) {
            strategy = DERIVE_STOCK;
            i++;
        } else if (i < clause_end && token_is(&tokens[i], T_VARID, "anyclass")) {
            strategy = DERIVE_ANYCLASS;
            i++;
        } else if (i < clause_end && token_is(&tokens[i], T_RESERVED, "newtype")) {
            strategy = DERIVE_NEWTYPE;
            i++;
        }
        for (j = i; j < clause_end; j++) {
            if (token_is(&tokens[j], T_VARID, "via")) {
                strategy = DERIVE_VIA;
            }
        }
        /* A class name is the first token of the clause, or the token after
           the opening parenthesis or after a comma. */
        for (j = i; j < clause_end && !token_is(&tokens[j], T_VARID, "via");
             j++) {
            int starts_item = j == i
                || (depth == 1 && (token_is(&tokens[j - 1], T_SPECIAL, ",")
                                   || token_is(&tokens[j - 1], T_SPECIAL, "(")));

            if (starts_item && tokens[j].kind == T_CONID) {
                add_derived_instance(module, type, j, strategy);
            }
            if (is_open_bracket(&tokens[j])) {
                depth++;
            } else if (is_close_bracket(&tokens[j])) {
                depth--;
            }
        }
        i = clause_end;
    }
}

/* data T a = A a Int | B        newtype N a = N [a]

   Returns 0 for forms that are not supported yet (infix constructors,
   GADTs, ...). The type is then known but none of its
   constructors are. */
static int add_data_declaration(Module *module, int start, int end)
{
    const Token *tokens = module->tokens;
    int is_newtype = token_is(&tokens[start], T_RESERVED, "newtype");
    Constructor **constructors = NULL;
    int count = 0;
    int i = start + 1;
    int j;
    TypeCon *type = add_type_head(module, &i, end);

    if (type == NULL) {
        return 0;
    }
    if (i == end || token_is(&tokens[i], T_RESERVED, "deriving")) {
        return 1; /* a type without constructors */
    }
    if (!token_is(&tokens[i], T_RESERVED, "=")) {
        return 0;
    }
    while (i < end && !token_is(&tokens[i], T_RESERVED, "deriving")) {
        Constructor *constructor;

        i++; /* the = or | before the constructor */
        constructor = read_constructor(module, &i, end);
        if (constructor == NULL) {
            return 0;
        }
        constructor->type = type;
        constructor->tag = count;
        constructor->is_newtype = is_newtype;
        constructors = xrealloc(constructors,
                                (size_t)(count + 1) * sizeof(Constructor *));
        constructors[count++] = constructor;
    }
    type->constructors = constructors;
    type->constructor_count = count;
    add_deriving_clauses(module, type, i, end);
    for (j = 0; j < count; j++) {
        Constructor *constructor = constructors[j];
        int k;

        add_entity(module, NS_CONSTRUCTOR, constructor->name, type->name)
            ->constructor = constructor;
        /* Each field name is also a function that selects the field. */
        for (k = 0; k < constructor->arity; k++) {
            const char *name = constructor->field_names[k];

            if (name != NULL && find_entity(module, NS_VARIABLE, name) == NULL) {
                Entity *entity = add_entity(module, NS_VARIABLE, name, type->name);

                entity->global = new_global(module, name);
                entity->global->selector = type;
            }
        }
    }
    return 1;
}

/* type T a = right-hand side */
static int add_synonym_declaration(Module *module, int start, int end)
{
    int i = start + 1;
    TypeCon *type = add_type_head(module, &i, end);

    if (type == NULL) {
        return 0; /* type family, type instance */
    }
    if (i == end || !token_is(&module->tokens[i], T_RESERVED, "=")) {
        return 0;
    }
    type->is_synonym = 1;
    type->rhs_start = i + 1;
    type->rhs_end = end;
    return 1;
}

/* Records the signature `name, (+) :: type` for each of its names. With a
   class, the names are its methods. */
static void add_signature(Module *module, int start, int colon, int end,
                          Class *class)
{
    const Token *tokens = module->tokens;
    int i;

    for (i = start; i < colon; i++) {
        if (tokens[i].kind == T_VARID || tokens[i].kind == T_VARSYM) {
            Entity *entity = find_or_add_variable(module, tokens[i].text);

            entity->global->signature_start = colon + 1;
            entity->global->signature_end = end;
            if (class != NULL) {
                entity->global->class = class;
                entity->parent = class->name;
            }
        }
    }
}

/* A type signature or a clause of a top-level variable. Returns 0 for
   anything else. */
static int add_variable_declaration(Module *module, int start, int end)
{
    int colon = signature_colon(module->tokens, start, end);
    const char *name;

    if (colon >= 0) {
        add_signature(module, start, colon, end, NULL);
        return 1;
    }
    name = clause_name(module->tokens, start, end);
    if (name == NULL) {
        return 0;
    }
    add_clause(find_or_add_variable(module, name)->global, start, end);
    return 1;
}

/* If the declaration header in [start, end) has a context `(C a, D b) =>`,
   the position after it. Otherwise `start`. */
static int skip_context(const Token *tokens, int start, int end)
{
    int arrow = find_reserved(tokens, start, end, "=>");

    return arrow == end ? start : arrow + 1;
}

/* class Context => C a where { method signatures and default definitions }

   Returns 0 for forms that are not supported yet: several parameters,
   functional dependencies, kind signatures. */
static int add_class_declaration(Module *module, int start, int end)
{
    const Token *tokens = module->tokens;
    int where = find_reserved(tokens, start, end, "where");
    int i = skip_context(tokens, start + 1, where);
    int column;
    Class *class;

    if (where - i != 2 || tokens[i].kind != T_CONID
        || tokens[i + 1].kind != T_VARID) {
        return 0;
    }
    if (where + 1 < end && token_is(&tokens[where + 1], T_SPECIAL, "{")) {
        return 0;
    }
    class = xmalloc(sizeof(Class));
    class->name = tokens[i].text;
    class->module = module;
    class->variable = new_binding(tokens[i + 1].text);
    add_entity(module, NS_TYPE, class->name, NULL)->class = class;

    if (where + 1 >= end) {
        return 1; /* a class without methods */
    }
    column = tokens[where + 1].col;
    for (i = where + 1; i < end;) {
        int next = item_end(tokens, i, end, column);
        int colon = signature_colon(tokens, i, next);
        const char *name = clause_name(tokens, i, next);

        if (token_is(&tokens[i], T_RESERVED, "type")) {
            /* An associated type: type F a [:: kind] */
            int position = i + 1;
            TypeCon *family = add_type_head(module, &position, next);

            if (family != NULL) {
                family->is_family = 1;
                /* Importing C (..) brings the associated types of C. */
                find_entity(module, NS_TYPE, family->name)->parent =
                    class->name;
            }
        } else if (colon >= 0) {
            add_signature(module, i, colon, next, class);
        } else if (is_fixity_keyword(&tokens[i])) {
            add_fixity_declaration(module, i, next);
        } else if (name != NULL) {
            /* The default definition of a method. */
            add_clause(find_or_add_variable(module, name)->global, i, next);
        }
        /* Anything else is ignored for now. */
        i = next;
    }
    return 1;
}

/* The definition of the method in the instance, added if it is new. */
static Global *find_or_add_method(Instance *instance, const char *name)
{
    Global *global;

    for (global = instance->methods; global != NULL; global = global->next) {
        if (strcmp(global->name, name) == 0) {
            return global;
        }
    }
    global = new_global(instance->module, name);
    global->instance = instance;
    global->next = instance->methods;
    instance->methods = global;
    return global;
}

/* In an instance: type F (T a) = right-hand side */
static void add_family_instance(Module *module, int start, int end)
{
    int equals = find_reserved(module->tokens, start, end, "=");
    FamilyInstance *instance;

    if (equals == end) {
        return;
    }
    instance = xmalloc(sizeof(FamilyInstance));
    memset(instance, 0, sizeof(FamilyInstance));
    instance->module = module;
    instance->lhs_start = start;
    instance->lhs_end = equals;
    instance->rhs_start = equals + 1;
    instance->rhs_end = end;
    instance->next = all_family_instances;
    all_family_instances = instance;
}

/* instance Context => C (T a) where { method definitions } */
static int add_instance_declaration(Module *module, int start, int end)
{
    const Token *tokens = module->tokens;
    int where = find_reserved(tokens, start, end, "where");
    int i = skip_context(tokens, start + 1, where);
    int column;
    Instance *instance;

    if (i >= where || tokens[i].kind != T_CONID) {
        return 0;
    }
    if (where + 1 < end && token_is(&tokens[where + 1], T_SPECIAL, "{")) {
        return 0;
    }
    instance = xmalloc(sizeof(Instance));
    memset(instance, 0, sizeof(Instance));
    instance->module = module;
    instance->class_token = i;
    instance->head_start = i + 1;
    instance->head_end = where;
    instance->next = all_instances;
    all_instances = instance;

    if (where + 1 >= end) {
        return 1; /* an instance without definitions */
    }
    column = tokens[where + 1].col;
    for (i = where + 1; i < end;) {
        int next = item_end(tokens, i, end, column);
        const char *name = clause_name(tokens, i, next);

        if (token_is(&tokens[i], T_RESERVED, "type")) {
            add_family_instance(module, i + 1, next);
        } else if (name != NULL && signature_colon(tokens, i, next) < 0) {
            add_clause(find_or_add_method(instance, name), i, next);
        }
        /* Anything else (signatures, methods defined infix) is ignored for
           now. */
        i = next;
    }
    return 1;
}

/* Records one top-level declaration without parsing its body. A declaration
   we cannot handle is only remembered: it is not an error until a name it
   defines is needed (see note_unsupported). */
static void add_declaration(Module *module, int start, int end)
{
    const Token *first = &module->tokens[start];
    int supported = 1;

    if (token_is(first, T_RESERVED, "import")) {
        add_import_declaration(module, start, end);
    } else if (token_is(first, T_RESERVED, "foreign")) {
        add_foreign_declaration(module, start, end);
    } else if (is_fixity_keyword(first)) {
        add_fixity_declaration(module, start, end);
    } else if (token_is(first, T_RESERVED, "data")
               || token_is(first, T_RESERVED, "newtype")) {
        supported = add_data_declaration(module, start, end);
    } else if (token_is(first, T_RESERVED, "type")) {
        supported = add_synonym_declaration(module, start, end);
    } else if (token_is(first, T_RESERVED, "class")) {
        supported = add_class_declaration(module, start, end);
    } else if (token_is(first, T_RESERVED, "instance")) {
        supported = add_instance_declaration(module, start, end);
    } else {
        supported = add_variable_declaration(module, start, end);
    }
    if (!supported) {
        add_unsupported(module, start, end);
    }
}

/* The layout rule for the top level: a token that is first on its line and
   in the same column as the first declaration starts a new declaration. */
static void split_declarations(Module *module, int position)
{
    const Token *tokens = module->tokens;
    int column = tokens[position].col;
    int end = position;

    if (token_is(&tokens[position], T_SPECIAL, "{")) {
        TODO_AT(&tokens[position], "module body in explicit braces");
    }
    while (tokens[end].kind != T_EOF) {
        end++;
    }
    while (position < end) {
        int next = item_end(tokens, position, end, column);

        add_declaration(module, position, next);
        position = next;
    }
}

static int imports_module(const Module *module, const char *name)
{
    const Import *import;

    for (import = module->imports; import != NULL; import = import->next) {
        if (strcmp(import->name, name) == 0) {
            return 1;
        }
    }
    return 0;
}

static Module *parse_module(const char *file, const char *source,
                            int overloaded_strings)
{
    Module *module = xmalloc(sizeof(Module));
    const char **extensions;
    Token *tokens = lex(file, source, &extensions);
    int position = 0;
    int i;

    memset(module, 0, sizeof(Module));
    module->name = "Main";
    module->tokens = tokens;
    module->overloaded_strings = overloaded_strings;
    for (i = 0; extensions[i] != NULL; i++) {
        if (strcmp(extensions[i], "OverloadedStrings") == 0) {
            module->overloaded_strings = 1;
        } else if (strcmp(extensions[i], "NoOverloadedStrings") == 0) {
            module->overloaded_strings = 0;
        }
    }
    module->next = loaded_modules;
    loaded_modules = module;

    /* module Name (exports) where */
    if (token_is(&tokens[0], T_RESERVED, "module")) {
        module->name = tokens[1].text;
        if (token_is(&tokens[2], T_SPECIAL, "(")) {
            int where = 2;

            while (!token_is(&tokens[where], T_RESERVED, "where")
                   && tokens[where].kind != T_EOF) {
                where++;
            }
            module->exports = xmalloc(sizeof(Import));
            memset(module->exports, 0, sizeof(Import));
            module->exports->has_list = 1;
            add_import_items(module->exports, tokens, 2, where);
        }
        while (!token_is(&tokens[position], T_RESERVED, "where")) {
            if (tokens[position].kind == T_EOF) {
                die("%s: module header without where", file);
            }
            if (position > 1 && token_is(&tokens[position], T_RESERVED, "module")
                && strcmp(tokens[position + 1].text, module->name) != 0) {
                /* module A in the export list */
                module->reexports = xrealloc(
                    module->reexports,
                    (size_t)(module->reexport_count + 1) * sizeof(const char *));
                module->reexports[module->reexport_count++] =
                    tokens[position + 1].text;
            }
            position++;
        }
        position++;
    }
    split_declarations(module, position);
    /* Every module imports the Prelude unless it says how it wants it.
       NoImplicitPrelude is not handled yet. */
    if (strcmp(module->name, "Prelude") != 0
        && !imports_module(module, "Prelude")) {
        add_import(module, "Prelude");
    }
    return module;
}

Module *load_module_file(const char *path)
{
    char *source = read_file(path);

    if (source == NULL) {
        die("cannot open %s", path);
    }
    return parse_module(path, source, 0);
}

/* Data.Map in directory src is the file src/Data/Map.hs. */
static char *module_path(const char *directory, const char *name)
{
    char *path = xmalloc(strlen(directory) + strlen(name) + 5);
    char *p;

    strcpy(path, directory);
    strcat(path, "/");
    p = path + strlen(path);
    strcat(path, name);
    for (; *p != '\0'; p++) {
        if (*p == '.') {
            *p = '/';
        }
    }
    strcat(path, ".hs");
    return path;
}

Module *load_module(const char *name)
{
    Module *module;
    int i;

    for (module = loaded_modules; module != NULL; module = module->next) {
        if (strcmp(module->name, name) == 0) {
            return module;
        }
    }
    for (i = 0; i < search_directory_count; i++) {
        char *path = module_path(search_directories[i], name);
        char *source = read_file(path);

        if (source != NULL) {
            return parse_module(path, source,
                                search_directory_overloaded_strings[i]);
        }
    }
    die("module %s not found", name);
    return NULL;
}

Module *first_loaded_module(void)
{
    return loaded_modules;
}

int load_next_import(void)
{
    Module *module;
    Import *import;

    for (module = loaded_modules; module != NULL; module = module->next) {
        for (import = module->imports; import != NULL; import = import->next) {
            if (import->module == NULL && !import->attempted) {
                import->attempted = 1;
                imported_module(import);
                return 1;
            }
        }
    }
    return 0;
}

int load_all_imports(void)
{
    Module *module;
    Import *import;
    int loaded = 0;

    for (module = loaded_modules; module != NULL; module = module->next) {
        for (import = module->imports; import != NULL; import = import->next) {
            if (import->module == NULL) {
                imported_module(import);
                loaded = 1;
            }
        }
    }
    return loaded;
}
