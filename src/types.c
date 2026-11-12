#include "syntax.h"

#include <stdio.h>
#include <string.h>

/* Types: how they are represented, how they are parsed, and the type
   schemes of variables, constructors and instance methods. */

Binding *new_binding(const char *name)
{
    Binding *binding = xmalloc(sizeof(Binding));

    memset(binding, 0, sizeof(Binding));
    binding->name = name;
    return binding;
}

/* ---- Building types ---- */

static Type *new_type(enum TypeKind kind)
{
    Type *type = xmalloc(sizeof(Type));

    memset(type, 0, sizeof(Type));
    type->kind = kind;
    return type;
}

Type *new_meta(void)
{
    return new_type(TY_META);
}

Type *new_rigid(Binding *variable)
{
    Type *type = new_type(TY_RIGID);

    type->variable = variable;
    return type;
}

Type *new_type_constructor(TypeCon *constructor)
{
    Type *type = new_type(TY_CON);

    type->constructor = constructor;
    return type;
}

Type *new_type_application(Type *function, Type *argument)
{
    Type *type = new_type(TY_APP);

    type->function = function;
    type->argument = argument;
    return type;
}

Type *new_forall(int count, Binding **variables, Type *body)
{
    Type *type = new_type(TY_FORALL);

    type->variable_count = count;
    type->variables = variables;
    type->body = body;
    return type;
}

/* argument -> result is the type constructor -> applied to both. */
Type *function_type(Type *argument, Type *result)
{
    Type *arrow = new_type_constructor(&function_type_constructor);

    return new_type_application(new_type_application(arrow, argument), result);
}

int split_function_type(Type *type, Type **argument, Type **result)
{
    Type *inner;

    type = prune(type);
    if (type->kind != TY_APP) {
        return 0;
    }
    inner = prune(type->function);
    if (inner->kind != TY_APP) {
        return 0;
    }
    if (prune(inner->function)->kind != TY_CON
        || prune(inner->function)->constructor != &function_type_constructor) {
        return 0;
    }
    *argument = inner->argument;
    *result = type->argument;
    return 1;
}

Type *prune(Type *type)
{
    while (type->kind == TY_META && type->link != NULL) {
        type = type->link;
    }
    return type;
}

Type *substitute(Type *type, int count, Binding **variables, Type **types)
{
    int i;

    type = prune(type);
    switch (type->kind) {
    case TY_RIGID:
        for (i = 0; i < count; i++) {
            if (variables[i] == type->variable) {
                return types[i];
            }
        }
        return type;
    case TY_APP:
        return new_type_application(
            substitute(type->function, count, variables, types),
            substitute(type->argument, count, variables, types));
    case TY_FORALL:
        return new_forall(type->variable_count, type->variables,
                          substitute(type->body, count, variables, types));
    case TY_META:
    case TY_CON:
        break;
    }
    return type;
}

int types_equal(Type *a, Type *b)
{
    a = prune(a);
    b = prune(b);
    if (a->kind != b->kind) {
        return 0;
    }
    switch (a->kind) {
    case TY_CON:
        return a->constructor == b->constructor;
    case TY_APP:
        return types_equal(a->function, b->function)
            && types_equal(a->argument, b->argument);
    case TY_RIGID:
    case TY_META:
    case TY_FORALL:
        break;
    }
    return 0;
}

void print_type(Type *type)
{
    type = prune(type);
    switch (type->kind) {
    case TY_META:
        fprintf(stderr, "_");
        break;
    case TY_RIGID:
        fprintf(stderr, "%s", type->variable->name);
        break;
    case TY_CON:
        fprintf(stderr, "%s", type->constructor->name);
        break;
    case TY_APP:
        fprintf(stderr, "(");
        print_type(type->function);
        fprintf(stderr, " ");
        print_type(type->argument);
        fprintf(stderr, ")");
        break;
    case TY_FORALL:
        fprintf(stderr, "(forall. ");
        print_type(type->body);
        fprintf(stderr, ")");
        break;
    }
}

/* ---- Parsing types ---- */

typedef struct TypeParser {
    Module *module;
    const Token *tokens;
    int position;
    int end;
    Binding ***variables; /* the type variables in scope */
    int *count;
    int open;             /* whether unknown variables are added to them */
} TypeParser;

static Type *parse_function_type(TypeParser *parser);
static Type *synonym_rhs(TypeCon *synonym);

static int type_next_is(const TypeParser *parser, enum TokenKind kind,
                        const char *text)
{
    return parser->position < parser->end
        && token_is(&parser->tokens[parser->position], kind, text);
}

static int type_next_is_kind(const TypeParser *parser, enum TokenKind kind)
{
    return parser->position < parser->end
        && parser->tokens[parser->position].kind == kind;
}

static void type_expect(TypeParser *parser, const char *text)
{
    if (!type_next_is(parser, T_SPECIAL, text)) {
        TODO_AT(&parser->tokens[parser->position], "type syntax");
    }
    parser->position++;
}

static Binding *add_type_variable(TypeParser *parser, const char *name)
{
    Binding *variable = new_binding(name);

    *parser->variables = xrealloc(
        *parser->variables, (size_t)(*parser->count + 1) * sizeof(Binding *));
    (*parser->variables)[(*parser->count)++] = variable;
    return variable;
}

static Type *parse_type_variable(TypeParser *parser)
{
    const Token *token = &parser->tokens[parser->position];
    int i;

    parser->position++;
    for (i = 0; i < *parser->count; i++) {
        if (strcmp((*parser->variables)[i]->name, token->text) == 0) {
            return new_rigid((*parser->variables)[i]);
        }
    }
    if (!parser->open) {
        TODO_AT(token, "type variable that is not in scope");
    }
    return new_rigid(add_type_variable(parser, token->text));
}

static Type *parse_named_type(TypeParser *parser)
{
    const Token *token = &parser->tokens[parser->position];
    const char *qualifier;
    const char *name = split_qualified(token->text, &qualifier);
    Entity *entity = lookup_entity(parser->module, NS_TYPE, qualifier, name);

    parser->position++;
    if (entity == NULL) {
        unresolved(parser->module, token, token->text);
    }
    if (entity->type == NULL) {
        TODO_AT(token, "class used as a type");
    }
    if (entity->type->is_synonym && entity->type->parameter_count == 0) {
        /* A synonym without parameters is replaced wherever it stands. One
           with parameters is replaced in parse_type_application. */
        return synonym_rhs(entity->type);
    }
    return new_type_constructor(entity->type);
}

static int starts_type_atom(const TypeParser *parser)
{
    return type_next_is_kind(parser, T_VARID)
        || type_next_is_kind(parser, T_CONID)
        || type_next_is(parser, T_SPECIAL, "(")
        || type_next_is(parser, T_SPECIAL, "[");
}

static Type *parse_function_type(TypeParser *parser);
static int skip_type_context(const TypeParser *parser);

/* forall a b. [context =>] type, inside a type. Its variables are in
   scope only in it. */
static Type *parse_forall(TypeParser *parser)
{
    int outer = *parser->count;
    Binding **variables;
    Type *body;
    int count;
    int i;

    parser->position++;
    while (type_next_is_kind(parser, T_VARID)) {
        add_type_variable(parser, parser->tokens[parser->position].text);
        parser->position++;
    }
    count = *parser->count - outer;
    if (!type_next_is(parser, T_VARSYM, ".")) {
        TODO_AT(&parser->tokens[parser->position], "forall syntax");
    }
    parser->position++;
    /* The context ends before the parenthesis the forall is inside. */
    {
        int end = parser->end;
        int depth = 0;

        for (i = parser->position; i < end; i++) {
            const Token *token = &parser->tokens[i];

            if (token->kind == T_SPECIAL && strchr("([", token->text[0])) {
                depth++;
            } else if (token->kind == T_SPECIAL
                       && strchr(")]", token->text[0])) {
                if (depth == 0) {
                    break;
                }
                depth--;
            }
        }
        parser->end = i;
        parser->position = skip_type_context(parser);
        parser->end = end;
    }
    body = parse_function_type(parser);
    variables = xmalloc((size_t)count * sizeof(Binding *) + 1);
    for (i = 0; i < count; i++) {
        variables[i] = (*parser->variables)[outer + i];
    }
    /* The variables of the forall go out of scope. A variable that was
       first seen inside it, and is not one of them, belongs to the
       signature as a whole and stays. */
    for (i = outer + count; i < *parser->count; i++) {
        (*parser->variables)[i - count] = (*parser->variables)[i];
    }
    *parser->count -= count;
    return new_forall(count, variables, body);
}

/* A type that can be an argument without parentheses around it. */
static Type *parse_type_atom(TypeParser *parser)
{
    const Token *token = &parser->tokens[parser->position];
    Type *type;

    if (type_next_is(parser, T_VARID, "forall")) {
        return parse_forall(parser);
    } else if (type_next_is_kind(parser, T_VARID)) {
        return parse_type_variable(parser);
    } else if (type_next_is_kind(parser, T_CONID)) {
        return parse_named_type(parser);
    } else if (type_next_is(parser, T_SPECIAL, "(")) {
        parser->position++;
        if (type_next_is(parser, T_SPECIAL, ")")) {
            parser->position++;
            return new_type_constructor(&unit_type_constructor);
        }
        if (type_next_is(parser, T_RESERVED, "->")) {
            parser->position++;
            type_expect(parser, ")");
            return new_type_constructor(&function_type_constructor);
        }
        if (type_next_is(parser, T_SPECIAL, ",")) {
            /* (,) and (,,): the tuple types without their arguments. */
            int count = 1;

            while (type_next_is(parser, T_SPECIAL, ",")) {
                parser->position++;
                count++;
            }
            type_expect(parser, ")");
            return new_type_constructor(tuple_type_constructor(count));
        }
        type = parse_function_type(parser);
        if (type_next_is(parser, T_SPECIAL, ",")) {
            /* A tuple type: (a, b) is the type (,) applied to a and b. */
            Type **elements = xmalloc(MAX_TUPLE * sizeof(Type *));
            int count = 1;
            int i;

            elements[0] = type;
            while (type_next_is(parser, T_SPECIAL, ",")) {
                parser->position++;
                if (count == MAX_TUPLE) {
                    TODO_AT(token, "tuple with more elements than MAX_TUPLE");
                }
                elements[count++] = parse_function_type(parser);
            }
            type = new_type_constructor(tuple_type_constructor(count));
            for (i = 0; i < count; i++) {
                type = new_type_application(type, elements[i]);
            }
        }
        type_expect(parser, ")");
        return type;
    } else if (type_next_is(parser, T_SPECIAL, "[")) {
        parser->position++;
        if (type_next_is(parser, T_SPECIAL, "]")) {
            parser->position++;
            return new_type_constructor(&list_type_constructor);
        }
        type = parse_function_type(parser);
        type_expect(parser, "]");
        return new_type_application(
            new_type_constructor(&list_type_constructor), type);
    }
    TODO_AT(token, "type syntax");
    return NULL;
}

/* The right-hand side of a type synonym. */
static Type *synonym_rhs(TypeCon *synonym)
{
    if (synonym->rhs == NULL) {
        synonym->rhs = parse_type(synonym->module, synonym->rhs_start,
                                  synonym->rhs_end, &synonym->parameters,
                                  &synonym->parameter_count, 0);
    }
    return synonym->rhs;
}

/* T a b. Type synonyms are replaced by what they stand for right here, so
   the rest of the interpreter never sees one. */
static Type *parse_type_application(TypeParser *parser)
{
    const Token *token = &parser->tokens[parser->position];
    Type *head = parse_type_atom(parser);
    Type **arguments = NULL;
    int count = 0;
    int used = 0;
    int i;

    while (starts_type_atom(parser)) {
        arguments = xrealloc(arguments, (size_t)(count + 1) * sizeof(Type *));
        arguments[count++] = parse_type_atom(parser);
    }
    if (head->kind == TY_CON && head->constructor->is_synonym) {
        TypeCon *synonym = head->constructor;

        if (count < synonym->parameter_count) {
            TODO_AT(token, "type synonym without all its arguments");
        }
        head = substitute(synonym_rhs(synonym), synonym->parameter_count,
                          synonym->parameters, arguments);
        used = synonym->parameter_count;
    }
    for (i = used; i < count; i++) {
        head = new_type_application(head, arguments[i]);
    }
    return head;
}

static Type *parse_function_type(TypeParser *parser)
{
    Type *type = parse_type_application(parser);

    if (type_next_is(parser, T_RESERVED, "->")) {
        Type *result;
        int i;

        parser->position++;
        result = parse_function_type(parser);
        if (result->kind == TY_FORALL) {
            /* a -> (forall b. c) is forall b. a -> c: a forall on the
               result of a function belongs to the signature as a whole. */
            for (i = 0; i < result->variable_count; i++) {
                *parser->variables = xrealloc(
                    *parser->variables,
                    (size_t)(*parser->count + 1) * sizeof(Binding *));
                (*parser->variables)[(*parser->count)++] =
                    result->variables[i];
            }
            result = result->body;
        }
        return function_type(type, result);
    }
    return type;
}

/* The position after the context `(C a, D b) =>` of a type, if it has
   one. Contexts are not needed: see Instance in syntax.h. */
static int skip_type_context(const TypeParser *parser)
{
    int depth = 0;
    int i;

    for (i = parser->position; i < parser->end; i++) {
        const Token *token = &parser->tokens[i];

        if (token->kind == T_SPECIAL
            && (token->text[0] == '(' || token->text[0] == '[')) {
            depth++;
        } else if (token->kind == T_SPECIAL
                   && (token->text[0] == ')' || token->text[0] == ']')) {
            depth--;
        } else if (depth == 0 && token_is(token, T_RESERVED, "=>")) {
            return i + 1;
        }
    }
    return parser->position;
}

/* [forall a b.] [context =>] type */
Type *parse_type(Module *module, int start, int end, Binding ***variables,
                 int *count, int open)
{
    TypeParser parser;
    Type *type;

    parser.module = module;
    parser.tokens = module->tokens;
    parser.position = start;
    parser.end = end;
    parser.variables = variables;
    parser.count = count;
    parser.open = open;

    if (type_next_is(&parser, T_VARID, "forall")) {
        parser.position++;
        while (type_next_is_kind(&parser, T_VARID)) {
            add_type_variable(&parser, parser.tokens[parser.position].text);
            parser.position++;
        }
        if (!type_next_is(&parser, T_VARSYM, ".")) {
            TODO_AT(&parser.tokens[parser.position], "forall syntax");
        }
        parser.position++;
    }
    {
        /* The variables are in the order they are first written, the
           context included: `f @T` relies on that order. */
        int after_context = skip_type_context(&parser);
        int i;

        for (i = parser.position; parser.open && i < after_context; i++) {
            const Token *token = &parser.tokens[i];
            Binding *variable = NULL;
            int k;

            if (token->kind != T_VARID) {
                continue;
            }
            for (k = 0; k < *parser.count; k++) {
                if (strcmp((*parser.variables)[k]->name, token->text) == 0) {
                    variable = (*parser.variables)[k];
                }
            }
            if (variable == NULL) {
                variable = add_type_variable(&parser, token->text);
            }
            /* Fractional a, RealFloat a, ...: what defaulting needs to
               know about the context. */
            if (i > parser.position && (token - 1)->kind == T_CONID) {
                const char *class = (token - 1)->text;

                if (strcmp(class, "Fractional") == 0
                    || strcmp(class, "Floating") == 0
                    || strcmp(class, "RealFrac") == 0
                    || strcmp(class, "RealFloat") == 0) {
                    variable->is_fractional = 1;
                }
            }
        }
        parser.position = after_context;
    }
    type = parse_function_type(&parser);
    if (parser.position != parser.end) {
        TODO_AT(&parser.tokens[parser.position], "type syntax");
    }
    return type;
}

/* ---- Type schemes ---- */

static Scheme *new_scheme(int variable_count, Binding **variables, Type *type)
{
    Scheme *scheme = xmalloc(sizeof(Scheme));

    scheme->variable_count = variable_count;
    scheme->variables = variables;
    scheme->type = type;
    return scheme;
}

void prepare_instance(Instance *instance)
{
    const Token *token = &instance->module->tokens[instance->class_token];
    const char *qualifier;
    const char *name;
    Entity *entity;

    if (instance->class != NULL) {
        return;
    }
    name = split_qualified(token->text, &qualifier);
    entity = lookup_entity(instance->module, NS_TYPE, qualifier, name);
    if (entity == NULL || entity->class == NULL) {
        unresolved(instance->module, token, token->text);
    }
    if (instance->derived_type != NULL) {
        /* deriving on T a b: the head is T a b. For a class of types that
           hold values, it is T a: the instance is about b. */
        TypeCon *type = instance->derived_type;
        int count = type->parameter_count;
        int i;

        if (strcmp(name, "Functor") == 0 || strcmp(name, "Foldable") == 0
            || strcmp(name, "Traversable") == 0) {
            count--;
        }
        instance->head = new_type_constructor(type);
        for (i = 0; i < count; i++) {
            instance->head = new_type_application(
                instance->head, new_rigid(type->parameters[i]));
        }
        instance->variables = type->parameters;
        instance->variable_count = count;
    } else {
        instance->head = parse_type(instance->module, instance->head_start,
                                    instance->head_end, &instance->variables,
                                    &instance->variable_count, 1);
    }
    instance->class = entity->class;
}

/* The scheme of the definition of a method in an instance. For

       class C a where m :: forall b. a -> b -> a
       instance C (T x) where m = ...

   the definition has the type forall x b. T x -> b -> T x: the method's
   type with the instance head in place of the class variable. */
static Scheme *instance_method_scheme(Global *global)
{
    Instance *instance = global->instance;
    Entity *entity;
    Scheme *method;
    Binding **variables;
    int count;
    int i;

    prepare_instance(instance);
    entity = find_entity(instance->class->module, NS_VARIABLE, global->name);
    if (entity == NULL || entity->global->class != instance->class) {
        die("%s is not a method of class %s", global->name,
            instance->class->name);
    }
    method = global_scheme(entity->global);
    /* The method's own variables follow the class variable. */
    count = instance->variable_count + method->variable_count - 1;
    variables = xmalloc((size_t)count * sizeof(Binding *) + 1);
    for (i = 0; i < instance->variable_count; i++) {
        variables[i] = instance->variables[i];
    }
    for (i = 1; i < method->variable_count; i++) {
        variables[instance->variable_count + i - 1] = method->variables[i];
    }
    return new_scheme(count, variables,
                      substitute(method->type, 1, &instance->class->variable,
                                 &instance->head));
}

/* The type of a field of a constructor. A forall at its start is part of
   the type, not of a signature: the field holds a value that works at
   every type. */
static Type *parse_field_type(Module *module, int start, int end,
                              Binding ***variables, int *count)
{
    TypeParser parser;
    Type *type;

    if (!token_is(&module->tokens[start], T_VARID, "forall")) {
        return parse_type(module, start, end, variables, count, 0);
    }
    parser.module = module;
    parser.tokens = module->tokens;
    parser.position = start;
    parser.end = end;
    parser.variables = variables;
    parser.count = count;
    parser.open = 0;
    type = parse_forall(&parser);
    if (parser.position != parser.end) {
        TODO_AT(&parser.tokens[parser.position], "type syntax");
    }
    return type;
}

/* The scheme of a record field used as a function:
   forall parameters. T parameters -> field type */
static Scheme *selector_scheme(const Global *global)
{
    TypeCon *type_constructor = global->selector;
    int count = type_constructor->parameter_count;
    /* A copy: reading the field's type may add to it. */
    Binding **variables = xmalloc((size_t)count * sizeof(Binding *) + 1);
    Type *record = new_type_constructor(type_constructor);
    int i;
    int k;

    memcpy(variables, type_constructor->parameters,
           (size_t)count * sizeof(Binding *));
    for (i = 0; i < count; i++) {
        record = new_type_application(record, new_rigid(variables[i]));
    }
    for (i = 0; i < type_constructor->constructor_count; i++) {
        const Constructor *constructor = type_constructor->constructors[i];

        for (k = 0; k < constructor->arity; k++) {
            if (constructor->field_names[k] != NULL
                && strcmp(constructor->field_names[k], global->name) == 0) {
                Type *field = parse_field_type(constructor->module,
                                               constructor->field_starts[k],
                                               constructor->field_ends[k],
                                               &variables, &count);

                if (field->kind == TY_FORALL) {
                    /* field :: forall b. type. As with any function whose
                       result has a forall, the variables are the
                       selector's: forall parameters b. T parameters ->
                       type. */
                    Binding **all = xmalloc(
                        (size_t)(count + field->variable_count)
                        * sizeof(Binding *));

                    memcpy(all, variables, (size_t)count * sizeof(Binding *));
                    memcpy(all + count, field->variables,
                           (size_t)field->variable_count * sizeof(Binding *));
                    variables = all;
                    count += field->variable_count;
                    field = field->body;
                }
                return new_scheme(count, variables, function_type(record, field));
            }
        }
    }
    die("%s is not a field of %s", global->name, type_constructor->name);
    return NULL;
}

Scheme *global_scheme(Global *global)
{
    Binding **variables = NULL;
    int count = 0;
    Type *type;

    if (global->scheme != NULL) {
        return global->scheme;
    }
    if (global->instance != NULL) {
        global->scheme = instance_method_scheme(global);
    } else if (global->selector != NULL) {
        global->scheme = selector_scheme(global);
    } else if (global->signature_end > global->signature_start) {
        if (global->class != NULL) {
            /* A method: the class variable is its first type variable. */
            variables = xmalloc(sizeof(Binding *));
            variables[0] = global->class->variable;
            count = 1;
        }
        type = parse_type(global->module, global->signature_start,
                          global->signature_end, &variables, &count, 1);
        global->scheme = new_scheme(count, variables, type);
    } else {
        /* No signature: the type has to be inferred from the definition. */
        elaborate_global(global);
    }
    return global->scheme;
}

Scheme *constructor_scheme(Constructor *constructor)
{
    TypeCon *type_constructor = constructor->type;
    Binding **variables = type_constructor->parameters;
    int count = type_constructor->parameter_count;
    Type *type;
    int i;

    if (constructor->scheme != NULL) {
        return constructor->scheme;
    }
    if (type_constructor == &list_type_constructor) {
        /* [] :: forall a. [a]        (:) :: forall a. a -> [a] -> [a] */
        Binding *element = new_binding("a");

        variables = xmalloc(sizeof(Binding *));
        variables[0] = element;
        count = 1;
        type = new_type_application(new_type_constructor(type_constructor),
                                    new_rigid(element));
        if (constructor == &cons_constructor) {
            type = function_type(new_rigid(element), function_type(type, type));
        }
    } else {
        /* C :: forall parameters existentials. field types -> T parameters */
        type = new_type_constructor(type_constructor);
        for (i = 0; i < count; i++) {
            type = new_type_application(type, new_rigid(variables[i]));
        }
        variables = xmalloc(
            (size_t)(count + constructor->existential_count) * sizeof(Binding *)
            + 1);
        for (i = 0; i < count; i++) {
            variables[i] = type_constructor->parameters[i];
        }
        for (i = 0; i < constructor->existential_count; i++) {
            variables[count++] = constructor->existentials[i];
        }
        for (i = constructor->arity - 1; i >= 0; i--) {
            Type *field;

            if (constructor->module == NULL) {
                /* A tuple: each field has the type of one parameter. */
                field = new_rigid(variables[i]);
            } else {
                field = parse_field_type(constructor->module,
                                         constructor->field_starts[i],
                                         constructor->field_ends[i],
                                         &variables, &count);
            }

            type = function_type(field, type);
        }
    }
    constructor->scheme = new_scheme(count, variables, type);
    return constructor->scheme;
}
