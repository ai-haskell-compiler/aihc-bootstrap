#include "syntax.h"

#include <stdio.h>
#include <string.h>

/* Type inference. Its job is not to find type errors: the program has
   already been accepted by a real compiler. Its job is to find out at which
   types polymorphic things are used, and to write that into the program:

     - a use of `f :: forall a b. ...` becomes `f @a @b`, with the types as
       ordinary arguments (E_TYPE);
     - the definition of `f` becomes `\a b -> ...`, with the type variables
       as ordinary parameters;
     - a use of a class method becomes an E_METHOD that carries the type the
       class is used at. The instance is looked up at run time
       (instances.c), so there are no dictionaries and constraints are never
       solved.

   A constructor takes type arguments only for its existential variables;
   a pattern for it binds them again.

   Local bindings (E_LET) work like top-level ones. A local function without
   a signature gets the most general type, as Haskell says. To know which
   TY_METAs it may turn into type variables, every TY_META has a level: the
   number of lets it was made inside. A let generalises the TY_METAs of a
   deeper level than its own, and unification lowers levels when a type
   becomes reachable from outside. */

typedef struct Inference {
    Module *module;
    Binding **variables; /* the type variables an annotation may mention */
    int variable_count;
    Type **instantiated; /* the types the last use of a polymorphic name
                            was given for its type variables, in order */
    int instantiated_count;
    int visible_count;   /* how many of them `@type` has decided */
    Type **literals;     /* the type of each overloaded literal */
    const char **literal_fallbacks; /* "Integer" or "String": its type if
                                       nothing else decides it */
    int literal_count;
    int level;           /* how many lets deep we are */
    Binding *generalizing; /* the local function whose type is being
                              inferred, or NULL */
} Inference;

/* Binding.state */
enum {
    BINDING_UNTYPED,
    BINDING_INFERRING,
    BINDING_PENDING, /* inferred, and waiting to be generalised with the
                        function that leads its group */
    BINDING_TYPED
};

static Type *infer(Inference *inference, Expr *expr);

static Type *fresh(const Inference *inference)
{
    Type *type = new_meta();

    type->level = inference->level;
    return type;
}

/* Makes sure no TY_META in the type has a deeper level than `level`. */
static void lower_levels(Type *type, int level)
{
    type = prune(type);
    if (type->kind == TY_APP) {
        lower_levels(type->function, level);
        lower_levels(type->argument, level);
    } else if (type->kind == TY_FORALL) {
        lower_levels(type->body, level);
    } else if (type->kind == TY_META && type->level > level) {
        type->level = level;
    }
}

static void mismatch(Type *a, Type *b, const Token *token)
{
    if (!has_failure_handler()) {
        fflush(stdout);
        fprintf(stderr, "type mismatch: ");
        print_type(a);
        fprintf(stderr, " and ");
        print_type(b);
        fprintf(stderr, "\n");
    }
    /* The program is known to be well typed, so this is our mistake. */
    TODO_AT(token, "type mismatch: the type pass is wrong or incomplete");
}

/* Makes the two types equal by filling in TY_METAs. */
static void unify(Type *a, Type *b, const Token *token)
{
    a = prune(a);
    b = prune(b);
    if (a == b) {
        return;
    }
    if (a->kind == TY_META) {
        lower_levels(b, a->level);
        a->link = b;
    } else if (b->kind == TY_META) {
        lower_levels(a, b->level);
        b->link = a;
    } else if (is_family_application(a) || is_family_application(b)) {
        /* An associated type is equal to whatever it is used as: the
           program is known to be well typed. If it is known what it stands
           for, that is used; otherwise it is found out when the program
           runs. */
        Type *known_a = reduce_family(a);
        Type *known_b = reduce_family(b);

        if (known_a != a || known_b != b) {
            unify(known_a, known_b, token);
        }
    } else if (a->kind == TY_RIGID && b->kind == TY_RIGID
               && a->variable == b->variable) {
        return;
    } else if (a->kind == TY_CON && b->kind == TY_CON
               && a->constructor == b->constructor) {
        return;
    } else if (a->kind == TY_APP && b->kind == TY_APP) {
        unify(a->function, b->function, token);
        unify(a->argument, b->argument, token);
    } else if (a->kind == TY_FORALL && b->kind == TY_FORALL
               && a->variable_count == b->variable_count) {
        /* The same, with the variables of one named as those of the
           other. */
        Type **same = xmalloc((size_t)a->variable_count * sizeof(Type *) + 1);
        int i;

        for (i = 0; i < a->variable_count; i++) {
            same[i] = new_rigid(a->variables[i]);
        }
        unify(a->body,
              substitute(b->body, b->variable_count, b->variables, same),
              token);
    } else {
        mismatch(a, b, token);
    }
}

/* Notes that the type is `fallback` if nothing else decides it. */
static void add_default(Inference *inference, Type *type, const char *fallback)
{
    inference->literals = xrealloc(
        inference->literals,
        (size_t)(inference->literal_count + 1) * sizeof(Type *));
    inference->literal_fallbacks = xrealloc(
        inference->literal_fallbacks,
        (size_t)(inference->literal_count + 1) * sizeof(const char *));
    inference->literals[inference->literal_count] = type;
    inference->literal_fallbacks[inference->literal_count] = fallback;
    inference->literal_count++;
}

/* Whether the class is one that only fractional number types are in. A
   type that is used at such a class, and that nothing else decides, is
   Double: that is Haskell's defaulting rule, for the classes there are. */
static int is_fractional_class(const Class *class)
{
    static const char *names[] = {
        "Fractional", "Floating", "RealFrac", "RealFloat", NULL
    };
    int i;

    for (i = 0; names[i] != NULL; i++) {
        if (strcmp(class->name, names[i]) == 0) {
            return 1;
        }
    }
    return 0;
}

/* The type of the scheme with a fresh TY_META for each of its variables.
   `*metas` receives those. */
static Type *instantiate(Inference *inference, const Scheme *scheme,
                         Type ***metas)
{
    int i;

    *metas = xmalloc((size_t)scheme->variable_count * sizeof(Type *) + 1);
    for (i = 0; i < scheme->variable_count; i++) {
        (*metas)[i] = fresh(inference);
    }
    for (i = 0; i < scheme->variable_count; i++) {
        if (scheme->variables[i]->is_fractional) {
            add_default(inference, (*metas)[i], "Double");
        }
    }
    /* For `name @type`: see E_VISIBLE. */
    inference->instantiated = *metas;
    inference->instantiated_count = scheme->variable_count;
    inference->visible_count = 0;
    return substitute(scheme->type, scheme->variable_count, scheme->variables,
                      *metas);
}

static Type *prelude_type(const char *name)
{
    Entity *entity = lookup_entity(NULL, NS_TYPE, PRELUDE_NAME, name);

    if (entity == NULL || entity->type == NULL) {
        die("the Prelude does not define the type %s", name);
    }
    return new_type_constructor(entity->type);
}

static Expr *copy_expr(const Expr *expr)
{
    Expr *copy = xmalloc(sizeof(Expr));

    *copy = *expr;
    return copy;
}

/* Rewrites `expr` in place to `expr @types[first] @... @types[count - 1]`. */
static void add_type_arguments(Expr *expr, Type **types, int first, int count)
{
    Expr *result;
    int i;

    if (first == count) {
        return;
    }
    result = copy_expr(expr);
    for (i = first; i < count; i++) {
        Expr *argument = copy_expr(expr);
        Expr *application = copy_expr(expr);

        memset(argument, 0, sizeof(Expr));
        argument->kind = E_TYPE;
        argument->token = expr->token;
        argument->type = types[i];
        memset(application, 0, sizeof(Expr));
        application->kind = E_APP;
        application->token = expr->token;
        application->function = result;
        application->argument = argument;
        result = application;
    }
    *expr = *result;
}

/* The type of the constructor of the pattern. The parameters of its type
   are unknown, as in an expression. Its existential variables are not: each
   becomes a type variable of its own, which the pattern binds at run time
   to the type stored in the value. */
static Type *instantiate_pattern(const Inference *inference, Pattern *pattern)
{
    const Constructor *constructor = pattern->constructor;
    const Scheme *scheme = constructor_scheme(pattern->constructor);
    int parameter_count = scheme->variable_count - constructor->existential_count;
    Type **types = xmalloc((size_t)scheme->variable_count * sizeof(Type *) + 1);
    int i;

    pattern->existentials = xmalloc(
        (size_t)constructor->existential_count * sizeof(Binding *) + 1);
    for (i = 0; i < scheme->variable_count; i++) {
        if (i < parameter_count) {
            types[i] = fresh(inference);
        } else {
            Binding *variable = new_binding(scheme->variables[i]->name);

            pattern->existentials[i - parameter_count] = variable;
            types[i] = new_rigid(variable);
        }
    }
    return substitute(scheme->type, scheme->variable_count, scheme->variables,
                      types);
}

/* Rewrites the literal `expr` in place to `convert literal`, where convert
   is the class method that gives a literal the type it is used at:
   fromInteger for a number, fromString for a string under
   OverloadedStrings. Returns that type. */
static Type *infer_overloaded_literal(Inference *inference, Expr *expr,
                                      Global *convert, const char *fallback)
{
    Expr *literal = copy_expr(expr);
    Expr *method = copy_expr(expr);
    Type *type = fresh(inference);

    memset(method, 0, sizeof(Expr));
    method->kind = E_METHOD;
    method->token = expr->token;
    method->global = convert;
    method->type = type;
    expr->kind = E_APP;
    expr->function = method;
    expr->argument = literal;
    add_default(inference, type, fallback);
    return type;
}

static Type *string_type(void)
{
    return new_type_application(new_type_constructor(&list_type_constructor),
                                prelude_type("Char"));
}

/* The type of the pattern. Gives each of its variables a type. */
static Type *infer_pattern(Inference *inference, Pattern *pattern)
{
    Type *type;
    int i;

    switch (pattern->kind) {
    case P_VARIABLE:
        pattern->variable->type = fresh(inference);
        return pattern->variable->type;
    case P_WILDCARD:
        return fresh(inference);
    case P_CHAR:
        return prelude_type("Char");
    case P_LITERAL:
        /* The pattern matches what is == to the literal. */
        type = infer(inference, pattern->literal);
        pattern->equals = xmalloc(sizeof(Expr));
        memset(pattern->equals, 0, sizeof(Expr));
        pattern->equals->kind = E_METHOD;
        pattern->equals->token = pattern->token;
        pattern->equals->global =
            library_entity("Prelude", NS_VARIABLE, "==")->global;
        pattern->equals->type = type;
        return type;
    case P_CONSTRUCTOR:
        type = instantiate_pattern(inference, pattern);
        for (i = 0; i < pattern->argument_count; i++) {
            Type *argument;
            Type *result;

            if (!split_function_type(type, &argument, &result)) {
                die("%s:%d: constructor pattern with too many arguments",
                    pattern->token->file, pattern->token->line);
            }
            unify(infer_pattern(inference, pattern->arguments[i]), argument,
                  pattern->token);
            type = result;
        }
        return type;
    case P_AS:
        pattern->variable->type = infer_pattern(inference, pattern->inner);
        return pattern->variable->type;
    case P_BANG:
        return infer_pattern(inference, pattern->inner);
    case P_VIEW: {
        /* The function goes from the type of the value to the type of the
           inner pattern. */
        Type *argument = fresh(inference);
        Type *result = infer_pattern(inference, pattern->inner);

        unify(infer(inference, pattern->literal),
              function_type(argument, result), pattern->token);
        return argument;
    }
    case P_RECORD:
        break;
    }
    return NULL;
}

/* Turns every TY_META in the type that is deeper than `level` into a type
   variable, and adds those to `*variables`. */
static void generalize(Type *type, int level, Binding ***variables, int *count)
{
    type = prune(type);
    if (type->kind == TY_APP) {
        generalize(type->function, level, variables, count);
        generalize(type->argument, level, variables, count);
    } else if (type->kind == TY_FORALL) {
        generalize(type->body, level, variables, count);
    } else if (type->kind == TY_META && type->level > level) {
        Binding *variable = new_binding("t");

        *variables = xrealloc(*variables,
                              (size_t)(*count + 1) * sizeof(Binding *));
        (*variables)[(*count)++] = variable;
        type->link = new_rigid(variable);
    }
}

/* \variables -> expr: the type variables as parameters, the first one
   outermost. */
static Expr *add_type_parameters(Expr *expr, int count, Binding **variables)
{
    int i;

    for (i = count - 1; i >= 0; i--) {
        Expr *lambda = xmalloc(sizeof(Expr));

        memset(lambda, 0, sizeof(Expr));
        lambda->kind = E_LAMBDA;
        lambda->token = expr->token;
        lambda->local = variables[i];
        lambda->body = expr;
        expr = lambda;
    }
    return expr;
}

/* The scheme a local type signature gives. The type variables of the
   enclosing definition that are in scope are not its own. */
static Scheme *local_signature(const Inference *inference, const Binding *binding)
{
    Scheme *scheme = xmalloc(sizeof(Scheme));
    int outer = inference->variable_count;
    int count = outer;
    Binding **variables = xmalloc((size_t)outer * sizeof(Binding *) + 1);
    int i;

    for (i = 0; i < outer; i++) {
        variables[i] = inference->variables[i];
    }
    scheme->type = parse_type(inference->module, binding->signature_start,
                              binding->signature_end, &variables, &count, 1);
    scheme->variables = variables + outer;
    scheme->variable_count = count - outer;
    return scheme;
}

/* Infers the definition of a variable bound by let or where. */
/* Local functions that call each other.

   The type of a local function is generalised when its definition has been
   inferred. If the definition calls a function whose own definition is
   still being inferred (it is further out: the call is how we got here),
   the two depend on each other and neither can be generalised first. The
   outer one becomes the leader of the inner one, and of every function
   whose inference is in progress between them. A function with a leader is
   left as it is (BINDING_PENDING) until the leader is done; then all of
   them are generalised over the same type variables. Until then every use
   of them is at the one type they have so far, and is recorded, so that it
   can be given those type variables as arguments. */

/* The function that will generalise this one: itself if it has no leader. */
static Binding *group_leader(Binding *binding)
{
    while (binding->leader != NULL) {
        binding = binding->leader;
    }
    return binding;
}

static void add_member(Binding *leader, Binding *member)
{
    leader->members = xrealloc(
        leader->members,
        (size_t)(leader->member_count + 1) * sizeof(Binding *));
    leader->members[leader->member_count++] = member;
}

/* Makes the function a function of the type variables too, and completes
   the recorded uses of it. */
static void generalize_function(Binding *binding, int count,
                                Binding **variables)
{
    Type **types;
    int i;

    if (count == 0) {
        return;
    }
    types = xmalloc((size_t)count * sizeof(Type *));
    binding->scheme = xmalloc(sizeof(Scheme));
    binding->scheme->variable_count = count;
    binding->scheme->variables = variables;
    binding->scheme->type = binding->type;
    /* Inside the group, a function is used at the group's own type
       variables. */
    for (i = 0; i < count; i++) {
        types[i] = new_rigid(variables[i]);
    }
    for (i = 0; i < binding->use_count; i++) {
        add_type_arguments(binding->uses[i], types, 0, count);
    }
    *binding->definition =
        add_type_parameters(*binding->definition, count, variables);
}

static void give_parameter_types(Expr *expr, Type *type);

static void infer_binding(Inference *inference, Binding *binding)
{
    const Token *token = (*binding->definition)->token;
    int saved_level = inference->level;
    Binding *saved_generalizing = inference->generalizing;
    Binding **variables = NULL;
    int count = 0;
    int i;

    if (binding->state != BINDING_UNTYPED) {
        return;
    }
    binding->state = BINDING_INFERRING;
    if (binding->scheme != NULL) {
        /* It has a signature. */
        give_parameter_types(*binding->definition, binding->scheme->type);
        unify(infer(inference, *binding->definition), binding->scheme->type,
              token);
        *binding->definition = add_type_parameters(
            *binding->definition, binding->scheme->variable_count,
            binding->scheme->variables);
    } else if (binding->is_function) {
        /* f x = ...: infer one level deeper, then generalise. */
        Binding *leader;

        inference->level = binding->level + 1;
        inference->generalizing = binding;
        binding->outer = saved_generalizing;
        binding->leader = NULL;
        binding->type = fresh(inference);
        unify(binding->type, infer(inference, *binding->definition), token);
        inference->level = saved_level;
        inference->generalizing = saved_generalizing;
        leader = group_leader(binding);
        if (leader != binding) {
            /* It calls a function further out whose type is not known yet.
               That function generalises this one, and those this one
               leads, when its own definition is done. */
            add_member(leader, binding);
            for (i = 0; i < binding->member_count; i++) {
                add_member(leader, binding->members[i]);
            }
            binding->state = BINDING_PENDING;
            return;
        }
        /* One list of type variables for the whole group. */
        generalize(binding->type, binding->level, &variables, &count);
        for (i = 0; i < binding->member_count; i++) {
            generalize(binding->members[i]->type, binding->level, &variables,
                       &count);
        }
        generalize_function(binding, count, variables);
        for (i = 0; i < binding->member_count; i++) {
            generalize_function(binding->members[i], count, variables);
            binding->members[i]->state = BINDING_TYPED;
        }
    } else {
        /* x = ...: not generalised, as the monomorphism restriction says.
           (Haskell does generalise it when no class is involved; we cannot
           tell.) */
        inference->level = binding->level;
        unify(binding->type, infer(inference, *binding->definition), token);
        inference->level = saved_level;
    }
    binding->state = BINDING_TYPED;
}

/* The type of a use of a local variable. */
static Type *infer_local(Inference *inference, Expr *expr)
{
    Binding *binding = expr->local;
    Type **metas;
    Type *type;

    if (binding->definition != NULL && binding->scheme == NULL
        && binding->state == BINDING_UNTYPED) {
        /* A variable of a let that we have not come to yet. */
        infer_binding(inference, binding);
    }
    if (binding->scheme != NULL) {
        type = instantiate(inference, binding->scheme, &metas);
        add_type_arguments(expr, metas, 0, binding->scheme->variable_count);
        return type;
    }
    type = prune(binding->type);
    if (binding->scheme == NULL && type->kind == TY_FORALL) {
        /* A parameter that is a function of types: this use gives it
           types. */
        Scheme scheme;

        scheme.variable_count = type->variable_count;
        scheme.variables = type->variables;
        scheme.type = type->body;
        type = instantiate(inference, &scheme, &metas);
        add_type_arguments(expr, metas, 0, scheme.variable_count);
        return type;
    }
    if (binding->is_function && (binding->state == BINDING_INFERRING
                                 || binding->state == BINDING_PENDING)) {
        /* A local function whose type is not general yet: it calls itself,
           or it is in a group of functions that call each other. Every
           function whose definition is being inferred between here and
           the leader of that group joins the group. */
        Binding *leader = group_leader(binding);
        Binding *inner;

        for (inner = inference->generalizing; inner != NULL && inner != leader;
             inner = inner->outer) {
            inner->leader = leader;
        }
        binding->uses = xrealloc(
            binding->uses, (size_t)(binding->use_count + 1) * sizeof(Expr *));
        binding->uses[binding->use_count++] = expr;
    }
    return binding->type;
}

/* Infers the definitions of an E_LET. */
static void infer_let_bindings(Inference *inference, Expr *expr)
{
    int i;

    for (i = 0; i < expr->binding_count; i++) {
        Binding *binding = expr->bindings[i];

        binding->level = inference->level;
        binding->state = BINDING_UNTYPED;
        if (binding->signature_end > binding->signature_start) {
            binding->scheme = local_signature(inference, binding);
        } else {
            binding->type = fresh(inference);
        }
    }
    for (i = 0; i < expr->binding_count; i++) {
        infer_binding(inference, expr->bindings[i]);
    }
}

/* Infers one alternative of a case on a value of type `scrutinee` whose
   result has type `result`. */
static void infer_alternative(Inference *inference, Alternative *alternative,
                              Type *scrutinee, Type *result)
{
    Guard *guard;
    Qualifier *qualifier;

    unify(infer_pattern(inference, alternative->pattern), scrutinee,
          alternative->pattern->token);
    if (alternative->where != NULL) {
        infer_let_bindings(inference, alternative->where);
    }
    if (alternative->body != NULL) {
        unify(infer(inference, alternative->body), result,
              alternative->body->token);
    }
    for (guard = alternative->guards; guard != NULL; guard = guard->next) {
        for (qualifier = guard->qualifiers; qualifier != NULL;
             qualifier = qualifier->next) {
            Type *type = infer(inference, qualifier->expr);

            if (qualifier->pattern != NULL) {
                unify(infer_pattern(inference, qualifier->pattern), type,
                      qualifier->expr->token);
            } else {
                unify(type, prelude_type("Bool"), qualifier->expr->token);
            }
        }
        unify(infer(inference, guard->body), result, guard->body->token);
    }
}

/* Where a signature says a parameter has a forall type, the parameter gets
   that type before the definition is inferred, so that every use of the
   parameter can give it types of its own. Other parameters get their type
   by inference, as always. */
static void give_parameter_types(Expr *expr, Type *type)
{
    Type *parameter;
    Type *result;

    while (expr->kind == E_LAMBDA
           && split_function_type(prune(type), &parameter, &result)) {
        if (prune(parameter)->kind == TY_FORALL) {
            expr->local->type = prune(parameter);
        }
        expr = expr->body;
        type = result;
    }
}

/* The type written in an expression: `e :: type` or `f @type`. It may
   mention the type variables in scope. Any other type variable stands for
   a type that is still to be found out. */
static Type *annotation_type(Inference *inference, int start, int end)
{
    int known = inference->variable_count;
    int count = known;
    Binding **variables = xmalloc((size_t)known * sizeof(Binding *) + 1);
    Type **unknown;
    Type *type;
    int i;

    if (known > 0) {
        memcpy(variables, inference->variables,
               (size_t)known * sizeof(Binding *));
    }
    type = parse_type(inference->module, start, end, &variables, &count, 1);
    if (count == known) {
        return type;
    }
    unknown = xmalloc((size_t)(count - known) * sizeof(Type *));
    for (i = 0; i < count - known; i++) {
        unknown[i] = fresh(inference);
    }
    return substitute(type, count - known, variables + known, unknown);
}

/* The type of the expression. Rewrites it as described at the top. */
static Type *infer(Inference *inference, Expr *expr)
{
    Alternative *alternative;
    Type **metas;
    Type *type;
    Type *result;

    switch (expr->kind) {
    case E_LOCAL:
        return infer_local(inference, expr);
    case E_LET:
        infer_let_bindings(inference, expr);
        return infer(inference, expr->body);
    case E_GLOBAL: {
        const Scheme *scheme = global_scheme(expr->global);

        type = instantiate(inference, scheme, &metas);
        if (expr->global->class != NULL) {
            /* The class variable is the first variable of a method. */
            expr->kind = E_METHOD;
            expr->type = metas[0];
            if (is_fractional_class(expr->global->class)) {
                add_default(inference, metas[0], "Double");
            }
            add_type_arguments(expr, metas, 1, scheme->variable_count);
        } else {
            add_type_arguments(expr, metas, 0, scheme->variable_count);
        }
        return type;
    }
    case E_CONSTRUCTOR: {
        const Scheme *scheme = constructor_scheme(expr->constructor);

        type = instantiate(inference, scheme, &metas);
        add_type_arguments(
            expr, metas,
            scheme->variable_count - expr->constructor->existential_count,
            scheme->variable_count);
        return type;
    }
    case E_CHAR:
        return prelude_type("Char");
    case E_STRING:
        if (inference->module->overloaded_strings && !expr->is_plain_string) {
            return infer_overloaded_literal(
                inference, expr,
                library_entity("Data.String", NS_VARIABLE, "fromString")->global,
                "String");
        }
        return string_type();
    case E_INT:
        return infer_overloaded_literal(
            inference, expr,
            library_entity("Prelude", NS_VARIABLE, "fromInteger")->global,
            "Integer");
    case E_APP: {
        Type *parameter;

        type = infer(inference, expr->function);
        if (split_function_type(prune(type), &parameter, &result)
            && prune(parameter)->kind == TY_FORALL) {
            /* The function wants an argument that works at every type:
               forall a. body. The argument is inferred with the type
               variables standing for themselves, and becomes a function
               of them. */
            parameter = prune(parameter);
            unify(infer(inference, expr->argument), parameter->body,
                  expr->token);
            expr->argument = add_type_parameters(
                expr->argument, parameter->variable_count,
                parameter->variables);
            return result;
        }
        result = fresh(inference);
        unify(type, function_type(infer(inference, expr->argument), result),
              expr->token);
        return result;
    }
    case E_LAMBDA:
        /* The type may have been given by a signature: see
           give_parameter_types. */
        if (expr->local->type == NULL) {
            expr->local->type = fresh(inference);
        }
        return function_type(expr->local->type, infer(inference, expr->body));
    case E_CASE:
        type = infer(inference, expr->scrutinee);
        result = fresh(inference);
        for (alternative = expr->alternatives; alternative != NULL;
             alternative = alternative->next) {
            infer_alternative(inference, alternative, type, result);
        }
        return result;
    case E_ANNOTATION:
        type = annotation_type(inference, expr->type_start, expr->type_end);
        unify(infer(inference, expr->body), type, expr->token);
        return type;
    case E_VISIBLE: {
        /* name @type: the type is what the next type variable of the name
           stands for here. The variables are in the order of the name's
           signature. */
        const Token *first = &inference->module->tokens[expr->type_start];
        int wildcard = expr->type_end == expr->type_start + 1
            && token_is(first, T_RESERVED, "_");

        type = infer(inference, expr->function);
        if (inference->visible_count >= inference->instantiated_count) {
            TODO_AT(expr->token, "type application to something that is not "
                                 "a name with type variables left");
        }
        if (!wildcard) {
            unify(inference->instantiated[inference->visible_count],
                  annotation_type(inference, expr->type_start, expr->type_end),
                  expr->token);
        }
        inference->visible_count++;
        *expr = *expr->function;
        return type;
    }
    case E_NAME:
    case E_OPERATORS:
    case E_RECORD:
    case E_TYPE:
    case E_METHOD:
        break;
    }
    die("the type pass met an expression it should never see");
    return NULL;
}

Expr *infer_global(Global *global, Expr *expr)
{
    const Token *tokens = global->module->tokens;
    Inference inference;
    Scheme *scheme = NULL;
    Type *type;
    int i;

    memset(&inference, 0, sizeof(Inference));
    inference.module = global->module;
    if (global->instance != NULL
        || global->signature_end > global->signature_start) {
        scheme = global_scheme(global);
        /* Which type variables of the signature the definition may mention:
           those of the instance head or the class, or all of them if the
           signature has a forall. */
        inference.variables = scheme->variables;
        if (global->instance != NULL) {
            inference.variable_count = global->instance->variable_count;
        } else if (global->class != NULL) {
            inference.variable_count = 1;
        } else if (token_is(&tokens[global->signature_start], T_VARID, "forall")) {
            inference.variable_count = scheme->variable_count;
        }
    }
    if (scheme != NULL) {
        give_parameter_types(expr, scheme->type);
    }
    type = infer(&inference, expr);
    if (scheme != NULL) {
        unify(type, scheme->type, expr->token);
    } else {
        scheme = xmalloc(sizeof(Scheme));
        scheme->variable_count = 0;
        scheme->variables = NULL;
        scheme->type = type;
        generalize(type, -1, &scheme->variables, &scheme->variable_count);
        global->scheme = scheme;
    }
    /* Defaulting: a number whose type nothing decides is a Double if it is
       used as a fraction and otherwise an Integer, and such a string is a
       String. The fractions go first, since a literal like 2 in 2 / 3 is
       noted as a whole number too. */
    for (i = 0; i < inference.literal_count; i++) {
        Type *literal = prune(inference.literals[i]);

        if (literal->kind == TY_META
            && strcmp(inference.literal_fallbacks[i], "Double") == 0) {
            literal->link = prelude_type("Double");
        }
    }
    for (i = 0; i < inference.literal_count; i++) {
        Type *literal = prune(inference.literals[i]);

        if (literal->kind == TY_META) {
            literal->link = strcmp(inference.literal_fallbacks[i], "String") == 0
                ? string_type()
                : prelude_type("Integer");
        }
    }
    return add_type_parameters(expr, scheme->variable_count, scheme->variables);
}
