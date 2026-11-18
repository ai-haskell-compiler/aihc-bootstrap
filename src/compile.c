#include "eval.h"

#include <stdlib.h>
#include <string.h>

/* Turns an elaborated definition (an Expr) into code for the machine: see
   Code in eval.h for what that is.

   Three things happen here:

     - Variables become slots. A Scope says which slot each variable in
       scope is in, for the function being compiled.
     - Every lambda and every expression that is kept for later (a thunk)
       becomes a Function of its own. The variables it mentions that are in
       scope around it are captured: copied into the closure when it is
       made, and from there into slots when it is run.
     - Patterns become tests. An alternative is compiled with the code to
       go to if it does not match, which is the next alternative. */

/* ---- The variables an expression mentions ---- */

/* The list also has the variables that are bound inside the expression;
   those are not in scope around it, and are skipped by whoever asks. */
typedef struct Mentioned {
    Binding **bindings;
    int count;
    int capacity;
} Mentioned;

static void note_expr(Mentioned *owner, const Expr *expr);

static void note_variable(Mentioned *owner, Binding *binding)
{
    int i;

    for (i = 0; i < owner->count; i++) {
        if (owner->bindings[i] == binding) {
            return;
        }
    }
    if (owner->count == owner->capacity) {
        owner->capacity = owner->capacity * 2 + 8;
        owner->bindings = xrealloc(owner->bindings, (size_t)owner->capacity
                                                        * sizeof(Binding *));
    }
    owner->bindings[owner->count++] = binding;
}

/* The same types as close_type in eval.c looks into. */
static void note_type(Mentioned *owner, Type *type)
{
    type = prune(type);
    if (type->kind == TY_RIGID) {
        note_variable(owner, type->variable);
    } else if (type->kind == TY_APP) {
        note_type(owner, type->function);
        note_type(owner, type->argument);
    }
}

static void note_pattern(Mentioned *owner, const Pattern *pattern)
{
    int i;

    if (pattern->literal != NULL) {
        note_expr(owner, pattern->literal);
    }
    if (pattern->equals != NULL) {
        note_expr(owner, pattern->equals);
    }
    if (pattern->inner != NULL) {
        note_pattern(owner, pattern->inner);
    }
    for (i = 0; i < pattern->argument_count; i++) {
        note_pattern(owner, pattern->arguments[i]);
    }
}

static void note_alternatives(Mentioned *owner, const Expr *expr)
{
    const Alternative *alternative;
    const Guard *guard;
    const Qualifier *qualifier;

    for (alternative = expr->alternatives; alternative != NULL;
         alternative = alternative->next) {
        note_pattern(owner, alternative->pattern);
        if (alternative->where != NULL) {
            note_expr(owner, alternative->where);
        }
        if (alternative->body != NULL) {
            note_expr(owner, alternative->body);
        }
        for (guard = alternative->guards; guard != NULL; guard = guard->next) {
            for (qualifier = guard->qualifiers; qualifier != NULL;
                 qualifier = qualifier->next) {
                if (qualifier->let != NULL) {
                    note_expr(owner, qualifier->let);
                    continue;
                }
                if (qualifier->pattern != NULL) {
                    note_pattern(owner, qualifier->pattern);
                }
                note_expr(owner, qualifier->expr);
            }
            note_expr(owner, guard->body);
        }
    }
}

static void note_expr(Mentioned *owner, const Expr *expr)
{
    int i;

    switch (expr->kind) {
    case E_LOCAL:
        note_variable(owner, expr->local);
        break;
    case E_APP:
        note_expr(owner, expr->function);
        note_expr(owner, expr->argument);
        break;
    case E_LAMBDA:
    case E_ANNOTATION:
        note_expr(owner, expr->body);
        break;
    case E_LET:
        for (i = 0; i < expr->binding_count; i++) {
            note_expr(owner, expr->values[i]);
        }
        if (expr->body != NULL) {
            note_expr(owner, expr->body);
        }
        break;
    case E_CASE:
        note_expr(owner, expr->scrutinee);
        note_alternatives(owner, expr);
        break;
    case E_TYPE:
    case E_METHOD:
        note_type(owner, expr->type);
        break;
    default:
        break;
    }
}

/* ---- Scopes ---- */

/* The function that is being compiled: which slot each variable in scope
   is in, and the code made for it so far. */
typedef struct Scope {
    struct Scope *outer; /* the function it is written in, or NULL */
    Binding **bindings;  /* the variables in scope, the innermost last */
    int *slots;
    int count;
    int capacity;
    int next_slot;       /* the first slot that nothing in scope is in */
    int slot_count;      /* the most slots that were ever taken */
    Code **nodes;        /* all its code, by Code.id */
    int node_count;
    int node_capacity;
} Scope;

/* Where a scope was, to go back to: what is bound after it goes out of
   scope, and its slots are free again. */
typedef struct Mark {
    int count;
    int next_slot;
} Mark;

static Mark mark_scope(const Scope *scope)
{
    Mark mark;

    mark.count = scope->count;
    mark.next_slot = scope->next_slot;
    return mark;
}

static void reset_scope(Scope *scope, Mark mark)
{
    scope->count = mark.count;
    scope->next_slot = mark.next_slot;
}

static int new_slot(Scope *scope)
{
    int slot = scope->next_slot++;

    if (scope->next_slot > scope->slot_count) {
        scope->slot_count = scope->next_slot;
    }
    return slot;
}

static void bind(Scope *scope, Binding *binding, int slot)
{
    if (scope->count == scope->capacity) {
        scope->capacity = scope->capacity * 2 + 16;
        scope->bindings = xrealloc(scope->bindings, (size_t)scope->capacity
                                                        * sizeof(Binding *));
        scope->slots = xrealloc(scope->slots,
                                (size_t)scope->capacity * sizeof(int));
    }
    scope->bindings[scope->count] = binding;
    scope->slots[scope->count] = slot;
    scope->count++;
}

/* The slot of the variable, or -1 if it is not in scope. */
static int find_slot(const Scope *scope, const Binding *binding)
{
    int i;

    if (scope == NULL) {
        return -1;
    }
    for (i = scope->count - 1; i >= 0; i--) {
        if (scope->bindings[i] == binding) {
            return scope->slots[i];
        }
    }
    return -1;
}

static Code *new_code(Scope *scope, enum CodeKind kind, const Token *token)
{
    Code *code = xmalloc(sizeof(Code));

    memset(code, 0, sizeof(Code));
    code->kind = (unsigned char)kind;
    code->token = token;
    code->id = scope->node_count;
    if (scope->node_count == scope->node_capacity) {
        scope->node_capacity = scope->node_capacity * 2 + 32;
        scope->nodes = xrealloc(scope->nodes, (size_t)scope->node_capacity
                                                  * sizeof(Code *));
    }
    scope->nodes[scope->node_count++] = code;
    return code;
}

static Code *new_local(Scope *scope, int slot, const Token *token)
{
    Code *code = new_code(scope, C_LOCAL, token);

    code->slot = slot;
    return code;
}

static Code *new_const(Scope *scope, Value *value, const Token *token)
{
    Code *code = new_code(scope, C_CONST, token);

    code->u.value = value;
    return code;
}

/* Code that fails when it is run. A variable that is not in scope is one:
   a definition that is never run may mention what it cannot have. */
static Code *new_failure(Scope *scope, const char *message, const Token *token)
{
    Code *code = new_code(scope, C_FAIL, token);

    code->u.string = message;
    return code;
}

/* `C_EVAL first`, into `slot`. What follows is still to be set. */
static Code *new_eval(Scope *scope, Code *first, int slot, const Token *token)
{
    Code *code = new_code(scope, C_EVAL, token);

    code->first = first;
    code->slot = slot;
    return code;
}

/* ---- Expressions ---- */

static Code *compile(Scope *scope, Expr *expr);
static Code *compile_delayed(Scope *scope, Expr *expr);
static Code *compile_scrutinee(Scope *scope, Expr *expr);
static void find_kills(Scope *scope, Function *function);
static void find_strictness(const Scope *scope, Function *function);

static Expr *without_annotations(Expr *expr)
{
    while (expr->kind == E_ANNOTATION) {
        expr = expr->body;
    }
    return expr;
}

/* The function for a lambda (with the lambdas its body starts with: one
   function of all their parameters), or for an expression that is kept
   for later (`thunk`): a C_CLOSURE that makes it with what it captures. A
   function that captures nothing is made once, here. */
static Code *compile_function(Scope *outer, Expr *expr, int thunk)
{
    Function *function = xmalloc(sizeof(Function));
    Mentioned mentioned;
    Scope scope;
    Code *code;
    int *captures;
    int count = 0;
    int i;

    memset(&mentioned, 0, sizeof(mentioned));
    memset(&scope, 0, sizeof(scope));
    memset(function, 0, sizeof(Function));
    scope.outer = outer;
    note_expr(&mentioned, expr);
    while (!thunk && expr->kind == E_LAMBDA) {
        bind(&scope, expr->local, new_slot(&scope));
        function->arity++;
        expr = expr->body;
    }
    captures = xmalloc((size_t)mentioned.count * sizeof(int) + 1);
    for (i = 0; i < mentioned.count; i++) {
        int slot = find_slot(outer, mentioned.bindings[i]);

        if (slot >= 0) {
            captures[count++] = slot;
            bind(&scope, mentioned.bindings[i], new_slot(&scope));
        }
    }
    free(mentioned.bindings);
    function->capture_count = count;
    if (thunk) {
        /* Its code is compiled when it is first run: see compile_body. */
        function->source = expr;
        function->bindings = scope.bindings;
        function->slot_count = count;
    } else {
        function->body = compile(&scope, expr);
        function->slot_count = scope.slot_count;
        find_kills(&scope, function);
        find_strictness(&scope, function);
        free(scope.bindings);
    }
    free(scope.slots);
    free(scope.nodes);
    if (count == 0 && function->arity > 0) {
        free(captures);
        /* Not a node of this function: it belongs to the one around it,
           or to none. */
        if (outer == NULL) {
            code = xmalloc(sizeof(Code));
            memset(code, 0, sizeof(Code));
            code->kind = C_CONST;
            code->u.value = constant_closure(function);
            return code;
        }
        return new_const(outer, constant_closure(function), expr->token);
    }
    if (outer == NULL) {
        code = xmalloc(sizeof(Code));
        memset(code, 0, sizeof(Code));
        code->kind = C_CLOSURE;
        code->token = expr->token;
    } else {
        code = new_code(outer, C_CLOSURE, expr->token);
    }
    code->u.function = function;
    code->slots = captures;
    code->count = count;
    return code;
}

void compile_body(Function *function)
{
    Scope scope;
    int i;

    memset(&scope, 0, sizeof(scope));
    for (i = 0; i < function->capture_count; i++) {
        bind(&scope, function->bindings[i], new_slot(&scope));
    }
    function->body = compile(&scope, function->source);
    function->slot_count = scope.slot_count;
    find_kills(&scope, function);
    free(scope.bindings);
    free(scope.slots);
    free(scope.nodes);
    free(function->bindings);
    function->bindings = NULL;
    function->source = NULL;
}

/* The type with every link followed. Type applications are made once, so
   a type without variables in it is one pointer to compare. */
static Type *follow_links(Type *type)
{
    Type *function;
    Type *argument;

    type = prune(type);
    if (type->kind != TY_APP) {
        return type;
    }
    function = follow_links(type->function);
    argument = follow_links(type->argument);
    if (function == type->function && argument == type->argument) {
        return type;
    }
    return new_type_application(function, argument);
}

/* Fills in the type of a C_TYPE or C_METHOD: the type, its variables, and
   the slots they are in. Returns 0 if one of them is not in scope. */
static int set_type(Scope *scope, Code *code, Type *type)
{
    Mentioned mentioned;
    int i;

    memset(&mentioned, 0, sizeof(mentioned));
    code->type = follow_links(type);
    note_type(&mentioned, code->type);
    code->count = mentioned.count;
    code->bindings = mentioned.bindings;
    code->slots = xmalloc((size_t)mentioned.count * sizeof(int) + 1);
    for (i = 0; i < mentioned.count; i++) {
        code->slots[i] = find_slot(scope, mentioned.bindings[i]);
        if (code->slots[i] < 0) {
            return 0;
        }
    }
    return 1;
}

static Code *compile_type(Scope *scope, Expr *expr)
{
    Code *code = new_code(scope, C_TYPE, expr->token);

    if (!set_type(scope, code, expr->type)) {
        return new_failure(scope, "a type variable is not in scope",
                           expr->token);
    }
    if (code->type->kind == TY_RIGID) {
        /* A type variable is the type it was given: nothing to work out. */
        return new_local(scope, code->slots[0], expr->token);
    }
    if (code->count == 0) {
        return new_const(scope, make_type(code->type), expr->token);
    }
    return code;
}

static Code *compile_method(Scope *scope, Expr *expr)
{
    Code *code = new_code(scope, C_METHOD, expr->token);

    code->u.global = expr->global;
    if (!set_type(scope, code, expr->type)) {
        return new_failure(scope, "a type variable is not in scope",
                           expr->token);
    }
    return code;
}

/* The constructor before it is given any fields. */
static Code *compile_constructor(Scope *scope, const Constructor *constructor,
                                 const Token *token)
{
    Value *value = constant_constructor(constructor);
    Code *code;

    if (value != NULL) {
        return new_const(scope, value, token);
    }
    code = new_code(scope, C_CONSTRUCT, token);
    code->tag = constructor->tag;
    return code;
}

/* Whether the code is a value that takes no work to get: see CodeKind. */
static int is_delayable(const Code *code)
{
    return code->kind <= C_TYPE;
}

/* An application as its function and its arguments in order. Returns the
   function and sets the rest. */
static Expr *split_application(Expr *expr, Expr ***arguments, int *count)
{
    Expr *head = expr;
    int n = 0;
    int i;

    while (head->kind == E_APP) {
        n++;
        head = without_annotations(head->function);
    }
    *arguments = xmalloc((size_t)n * sizeof(Expr *) + 1);
    *count = n;
    head = expr;
    for (i = n - 1; i >= 0; i--) {
        (*arguments)[i] = head->argument;
        head = without_annotations(head->function);
    }
    return head;
}

/* Whether the application gives the constructor all its fields (and the
   types it stores). */
static int is_saturated(const Expr *head, int count)
{
    return head->kind == E_CONSTRUCTOR
        && count == head->constructor->arity
                        + head->constructor->existential_count;
}

static int has_strict_fields(const Constructor *constructor)
{
    int i;

    for (i = 0; constructor->field_strict != NULL && i < constructor->arity;
         i++) {
        if (constructor->field_strict[i]) {
            return 1;
        }
    }
    return 0;
}

/* A constructor with all its fields. The strict ones are evaluated first,
   each into a slot of its own. */
static Code *compile_construction(Scope *scope, const Expr *head,
                                  Expr **arguments, int count)
{
    const Constructor *constructor = head->constructor;
    Code *construct = new_code(scope, C_CONSTRUCT, head->token);
    Code *start = construct;
    Code **last = &start;
    int i;

    construct->tag = constructor->tag;
    construct->count = count;
    construct->codes = xmalloc((size_t)count * sizeof(Code *) + 1);
    for (i = 0; i < count; i++) {
        int field = i - constructor->existential_count;

        if (field >= 0 && constructor->field_strict != NULL
            && constructor->field_strict[field]) {
            int slot = new_slot(scope);
            Code *eval = new_eval(scope, compile_scrutinee(scope, arguments[i]),
                                  slot, arguments[i]->token);

            *last = eval;
            last = &eval->next;
            construct->codes[i] = new_local(scope, slot, arguments[i]->token);
        } else {
            construct->codes[i] = compile_delayed(scope, arguments[i]);
        }
    }
    *last = construct;
    return start;
}

static int has_variables(Type *type)
{
    type = prune(type);
    switch (type->kind) {
    case TY_RIGID:
        return 1;
    case TY_APP:
        return has_variables(type->function) || has_variables(type->argument);
    default:
        return 0;
    }
}

/* Whether the expression is a number at a type that is known here: a
   number is written `fromInteger literal` (infer.c). It mentions no
   variable, so it stands for the same value every time. */
static int is_literal(const Expr *expr)
{
    return expr->kind == E_APP && expr->function->kind == E_METHOD
        && expr->argument->kind == E_INT
        && !has_variables(expr->function->type);
}

/* Says for each argument of the call which slot it is, or -1 if it is not
   a variable: most are, and the machine then does not look at their code. */
static void set_argument_slots(Code *call)
{
    int i;

    call->slots = xmalloc((size_t)call->count * sizeof(int) + 1);
    for (i = 0; i < call->count; i++) {
        call->slots[i] = call->codes[i]->kind == C_LOCAL
            ? call->codes[i]->slot : -1;
    }
}

/* The kind of small number that the class method is used at, if its
   type is known here and is one, and the class is the Prelude's `class`;
   else -1. */
static int method_number_kind(const Expr *method, const char *class)
{
    const Class *of = method->global->class;

    if (of->module == NULL || strcmp(of->module->name, "Prelude") != 0
        || strcmp(of->name, class) != 0 || has_variables(method->type)) {
        return -1;
    }
    return small_number_kind(method->type);
}

/* Arithmetic or a comparison of two small numbers of a type that is known
   here, as a C_PRIM; or NULL if the call is not that. The instances of
   the small number types define these methods as the builtins that a
   C_PRIM does the work of (lib/Prelude.hs). */
static Code *compile_primitive(Scope *scope, const Expr *head,
                               Expr **arguments)
{
    static const struct {
        const char *class;
        const char *name;
        char operation;
        int swapped;
    } known[] = {
        { "Num", "+", '+', 0 }, { "Num", "-", '-', 0 }, { "Num", "*", '*', 0 },
        { "Eq", "==", '=', 0 }, { "Eq", "/=", 'n', 0 },
        { "Ord", "<", '<', 0 }, { "Ord", "<=", 'l', 0 },
        { "Ord", ">", '<', 1 }, { "Ord", ">=", 'l', 1 },
        { "Ord", "compare", 'c', 0 }
    };
    Code *code;
    size_t i;
    int kind;

    for (i = 0; i < sizeof(known) / sizeof(known[0]); i++) {
        if (strcmp(head->global->name, known[i].name) == 0) {
            break;
        }
    }
    if (i == sizeof(known) / sizeof(known[0])) {
        return NULL;
    }
    kind = method_number_kind(head, known[i].class);
    if (kind < 0) {
        return NULL;
    }
    code = new_code(scope, C_PRIM, head->token);
    code->tag = known[i].operation;
    code->slot = kind;
    code->count = 2;
    code->codes = xmalloc(2 * sizeof(Code *));
    code->codes[0] = compile_delayed(scope, arguments[known[i].swapped]);
    code->codes[1] = compile_delayed(scope, arguments[!known[i].swapped]);
    set_argument_slots(code);
    return code;
}

/* A number literal at a type of small numbers that is known here: the
   number itself. NULL for any other expression. */
static Code *compile_small_number(Scope *scope, const Expr *expr)
{
    int kind;

    if (!is_literal(expr) || expr->argument->big != 0
        || strcmp(expr->function->global->name, "fromInteger") != 0) {
        return NULL;
    }
    kind = method_number_kind(expr->function, "Num");
    if (kind < 0) {
        return NULL;
    }
    return new_const(scope, number_constant(kind, expr->argument->number),
                     expr->token);
}

static Code *compile_application(Scope *scope, Expr *expr)
{
    Expr **arguments;
    int count;
    Expr *head;
    Code *call;
    Code *code;
    int i;

    code = compile_small_number(scope, expr);
    if (code != NULL) {
        return code;
    }
    head = split_application(expr, &arguments, &count);

    if (is_saturated(head, count)) {
        code = head->constructor->is_newtype
            ? compile(scope, arguments[0])
            : compile_construction(scope, head, arguments, count);
        free(arguments);
        return code;
    }
    if (head->kind == E_METHOD && count == 2) {
        code = compile_primitive(scope, head, arguments);
        if (code != NULL) {
            free(arguments);
            return code;
        }
    }
    call = new_code(scope, C_CALL, expr->token);
    code = call;
    switch (head->kind) {
    case E_LOCAL:
    case E_GLOBAL:
    case E_CONSTRUCTOR:
    case E_METHOD:
        call->first = compile(scope, head);
        break;
    default:
        break;
    }
    if (call->first == NULL
        || (call->first->kind != C_LOCAL && call->first->kind != C_GLOBAL
            && call->first->kind != C_CONST && call->first->kind != C_METHOD)) {
        /* The function takes work to get: it is evaluated first. */
        int slot = new_slot(scope);

        code = new_eval(scope, compile_scrutinee(scope, head), slot,
                        head->token);
        code->next = call;
        call->first = new_local(scope, slot, head->token);
    }
    call->count = count;
    call->codes = xmalloc((size_t)count * sizeof(Code *) + 1);
    for (i = 0; i < count; i++) {
        call->codes[i] = compile_delayed(scope, arguments[i]);
    }
    set_argument_slots(call);
    free(arguments);
    return code;
}

/* Code that gives the value of the expression without evaluating it: an
   argument, a field, the definition of a let. Anything that takes work
   becomes a thunk. */
static Code *compile_delayed(Scope *scope, Expr *expr)
{
    Code *code;

    expr = without_annotations(expr);
    code = compile_small_number(scope, expr);
    if (code != NULL) {
        return code;
    }
    if (is_literal(expr)) {
        /* Worked out once, the first time something needs it, and not
           every time this code is run. */
        code = compile_function(NULL, expr, 1);
        code->kind = C_CONST;
        code->u.value = shared_thunk(code->u.function);
        code->id = scope->node_count;
        if (scope->node_count == scope->node_capacity) {
            scope->node_capacity = scope->node_capacity * 2 + 32;
            scope->nodes = xrealloc(scope->nodes, (size_t)scope->node_capacity
                                                      * sizeof(Code *));
        }
        scope->nodes[scope->node_count++] = code;
        return code;
    }
    switch (expr->kind) {
    case E_LOCAL:
    case E_GLOBAL:
    case E_CONSTRUCTOR:
    case E_CHAR:
    case E_LAMBDA:
    case E_TYPE:
        code = compile(scope, expr);
        if (is_delayable(code)) {
            return code;
        }
        break;
    case E_INT:
        if (expr->big == 0) {
            return compile(scope, expr);
        }
        break;
    case E_APP: {
        Expr **arguments;
        int count;
        Expr *head = split_application(expr, &arguments, &count);

        code = NULL;
        if (is_saturated(head, count)) {
            if (head->constructor->is_newtype) {
                code = compile_delayed(scope, arguments[0]);
            } else if (!has_strict_fields(head->constructor)) {
                code = compile_construction(scope, head, arguments, count);
            }
        }
        free(arguments);
        if (code != NULL) {
            return code;
        }
        break;
    }
    default:
        break;
    }
    code = compile_function(scope, expr, 1);
    if (code->kind == C_CLOSURE && expr->kind == E_APP) {
        /* A call: also as code to run in place, for when the function it
           is an argument of always evaluates it. */
        Expr *head = expr;

        while (head->kind == E_APP) {
            head = without_annotations(head->function);
        }
        if (head->kind == E_LOCAL || head->kind == E_GLOBAL
            || head->kind == E_METHOD) {
            Code *call = compile_application(scope, expr);

            if (call->kind == C_CALL || call->kind == C_PRIM) {
                code->first = call;
            }
        }
    }
    return code;
}

/* Code for a C_EVAL to evaluate: a value that takes no work to get, a
   method, a call, or anything else as a function without parameters that
   is run on the spot. */
static Code *compile_scrutinee(Scope *scope, Expr *expr)
{
    Code *code;

    expr = without_annotations(expr);
    switch (expr->kind) {
    case E_LOCAL:
    case E_GLOBAL:
    case E_CONSTRUCTOR:
    case E_CHAR:
    case E_METHOD:
        code = compile(scope, expr);
        if (code->kind == C_LOCAL || code->kind == C_GLOBAL
            || code->kind == C_CONST || code->kind == C_METHOD) {
            return code;
        }
        break;
    case E_INT:
        if (expr->big == 0) {
            return compile(scope, expr);
        }
        break;
    case E_APP: {
        Expr *head = expr;

        while (head->kind == E_APP) {
            head = without_annotations(head->function);
        }
        if (head->kind == E_LOCAL || head->kind == E_GLOBAL
            || head->kind == E_METHOD) {
            code = compile_application(scope, expr);
            if (code->kind == C_CALL || code->kind == C_PRIM) {
                return code;
            }
        }
        break;
    }
    default:
        break;
    }
    code = compile_function(scope, expr, 1);
    if (code->kind == C_CLOSURE) {
        code->kind = C_ENTER;
    }
    return code;
}

/* ---- Let ---- */

/* What to compile once something is bound: the code that follows it. */
typedef Code *Continue(Scope *scope, void *data, Code *fail);

static int mentions_from(const Expr *expr, const Expr *let, int first)
{
    Mentioned mentioned;
    int found = 0;
    int i;
    int j;

    memset(&mentioned, 0, sizeof(mentioned));
    note_expr(&mentioned, expr);
    for (i = 0; i < mentioned.count && !found; i++) {
        for (j = first; j < let->binding_count; j++) {
            if (mentioned.bindings[i] == let->bindings[j]) {
                found = 1;
                break;
            }
        }
    }
    free(mentioned.bindings);
    return found;
}

/* The bindings of an E_LET, and then what `then` makes. Every definition
   can see all the bindings, itself included. The machine makes the
   closures and thunks first, then the other values in order, and then
   gives the closures and thunks what they capture: so a definition that
   mentions itself or a later binding has to be a thunk. A binding with a
   ! in front is evaluated before what follows, as in GHC; a program may
   rely on that to let go of what the expression refers to. */
static Code *compile_let(Scope *scope, Expr *let, Continue *then, void *data,
                         Code *fail)
{
    Code *code = new_code(scope, C_LET, let->token);
    Code **last = &code->next;
    int i;

    code->count = let->binding_count;
    code->slots = xmalloc((size_t)let->binding_count * sizeof(int) + 1);
    code->codes = xmalloc((size_t)let->binding_count * sizeof(Code *) + 1);
    for (i = 0; i < let->binding_count; i++) {
        code->slots[i] = new_slot(scope);
        bind(scope, let->bindings[i], code->slots[i]);
    }
    for (i = 0; i < let->binding_count; i++) {
        Expr *value = without_annotations(let->values[i]);

        if (value->kind == E_LAMBDA) {
            code->codes[i] = compile_function(scope, value, 0);
        } else if (mentions_from(value, let, i)) {
            code->codes[i] = compile_function(scope, value, 1);
        } else {
            code->codes[i] = compile_delayed(scope, value);
        }
    }
    for (i = 0; i < let->strict_count; i++) {
        int slot = find_slot(scope, let->strict[i]);
        Code *eval = new_eval(scope, new_local(scope, slot, let->token), slot,
                              let->token);

        *last = eval;
        last = &eval->next;
    }
    *last = then(scope, data, fail);
    return code;
}

static Code *let_body(Scope *scope, void *data, Code *fail)
{
    (void)fail;
    return compile(scope, data);
}

/* ---- Patterns ---- */

/* A value that a pattern is still to be matched against. */
typedef struct Obligation {
    const Pattern *pattern;
    int slot;      /* where the value is */
    int evaluated; /* it is known to be evaluated */
    int tag_known; /* a C_SWITCH has seen that it is the right constructor */
    struct Obligation *next;
} Obligation;

/* How many constructors the type of the constructor has. The types with
   syntax of their own do not list theirs (module.c). */
static int sibling_count(const Constructor *constructor)
{
    if (constructor->type == &list_type_constructor) {
        return 2;
    }
    if (constructor->type == &unit_type_constructor) {
        return 1;
    }
    return constructor->type->constructor_count;
}

/* Whether the pattern matches every value without evaluating it. */
static int matches_anything(const Pattern *pattern)
{
    switch (pattern->kind) {
    case P_VARIABLE:
    case P_WILDCARD:
        return 1;
    case P_AS:
        return matches_anything(pattern->inner);
    case P_CONSTRUCTOR:
        return pattern->constructor->is_newtype
            && matches_anything(pattern->arguments[0]);
    default:
        return 0;
    }
}

/* The constructor pattern that the pattern is once what does not test
   anything is taken off, or NULL. */
static const Pattern *constructor_pattern(const Pattern *pattern)
{
    for (;;) {
        switch (pattern->kind) {
        case P_AS:
        case P_BANG:
            pattern = pattern->inner;
            continue;
        case P_CONSTRUCTOR:
            if (pattern->constructor->is_newtype) {
                pattern = pattern->arguments[0];
                continue;
            }
            return pattern;
        default:
            return NULL;
        }
    }
}

static Expr *new_expr(enum ExprKind kind, const Token *token)
{
    Expr *expr = new_expr_memory();

    memset(expr, 0, sizeof(Expr));
    expr->kind = kind;
    expr->token = token;
    return expr;
}

/* The expression `function argument`, where the argument is the value in
   the slot. */
static Expr *apply_to_slot(Scope *scope, Expr *function, int slot)
{
    Expr *application = new_expr(E_APP, function->token);

    application->function = function;
    application->argument = new_expr(E_LOCAL, function->token);
    application->argument->local = new_binding("");
    bind(scope, application->argument->local, slot);
    return application;
}

/* Code that matches each value against its pattern, binds the variables
   of the patterns, and goes on with what `then` makes; or goes to `fail`
   if one of them does not match. */
static Code *compile_patterns(Scope *scope, Obligation *obligation,
                              Continue *then, void *data, Code *fail)
{
    const Pattern *pattern;
    Obligation inner;
    Code *code;
    Code *test;
    int slot;

    if (obligation == NULL) {
        return then(scope, data, fail);
    }
    pattern = obligation->pattern;
    slot = obligation->slot;
    inner = *obligation;
    switch (pattern->kind) {
    case P_VARIABLE:
        bind(scope, pattern->variable, slot);
        return compile_patterns(scope, obligation->next, then, data, fail);
    case P_WILDCARD:
        return compile_patterns(scope, obligation->next, then, data, fail);
    case P_AS:
        bind(scope, pattern->variable, slot);
        inner.pattern = pattern->inner;
        return compile_patterns(scope, &inner, then, data, fail);
    case P_BANG:
        inner.pattern = pattern->inner;
        inner.evaluated = 1;
        if (obligation->evaluated) {
            return compile_patterns(scope, &inner, then, data, fail);
        }
        code = new_eval(scope, new_local(scope, slot, pattern->token), slot,
                        pattern->token);
        code->next = compile_patterns(scope, &inner, then, data, fail);
        return code;
    case P_CHAR:
        test = new_code(scope, C_TEST_CHAR, pattern->token);
        test->slot = slot;
        test->tag = (int)pattern->number;
        test->next = fail;
        code = test;
        if (!obligation->evaluated) {
            code = new_eval(scope, new_local(scope, slot, pattern->token),
                            slot, pattern->token);
            code->next = test;
        }
        test->first = compile_patterns(scope, obligation->next, then, data,
                                       fail);
        return code;
    case P_LITERAL: {
        /* equals value literal */
        Expr *call = new_expr(E_APP, pattern->token);
        int condition = new_slot(scope);

        call->function = apply_to_slot(scope, pattern->equals, slot);
        call->argument = pattern->literal;
        code = new_eval(scope, compile_scrutinee(scope, call), condition,
                        pattern->token);
        test = new_code(scope, C_TEST, pattern->token);
        test->slot = condition;
        test->tag = TAG_TRUE;
        test->next = fail;
        code->next = test;
        test->first = compile_patterns(scope, obligation->next, then, data,
                                       fail);
        return code;
    }
    case P_VIEW: {
        int viewed = new_slot(scope);

        code = new_eval(scope,
                        compile_scrutinee(
                            scope, apply_to_slot(scope, pattern->literal, slot)),
                        viewed, pattern->token);
        inner.pattern = pattern->inner;
        inner.slot = viewed;
        inner.evaluated = 1;
        inner.tag_known = 0;
        code->next = compile_patterns(scope, &inner, then, data, fail);
        return code;
    }
    case P_CONSTRUCTOR: {
        const Constructor *constructor = pattern->constructor;
        int skip = constructor->existential_count;
        Obligation *fields;
        Obligation *rest = obligation->next;
        Code *start = NULL;
        Code **last = &start;
        Code *take;
        int count = 0;
        int i;

        if (constructor->is_newtype) {
            inner.pattern = pattern->arguments[0];
            return compile_patterns(scope, &inner, then, data, fail);
        }
        if (!obligation->evaluated && !obligation->tag_known) {
            code = new_eval(scope, new_local(scope, slot, pattern->token),
                            slot, pattern->token);
            *last = code;
            last = &code->next;
        }
        if (!obligation->tag_known && sibling_count(constructor) > 1) {
            test = new_code(scope, C_TEST, pattern->token);
            test->slot = slot;
            test->tag = constructor->tag;
            test->next = fail;
            *last = test;
            last = &test->first;
        }
        /* The fields that the patterns look at, and the types the value
           stores, each into a slot of its own. */
        take = new_code(scope, C_FIELDS, pattern->token);
        take->slot = slot;
        take->tag = scope->next_slot;
        take->slots = xmalloc((size_t)(skip + pattern->argument_count)
                                  * sizeof(int) + 1);
        fields = xmalloc((size_t)pattern->argument_count * sizeof(Obligation)
                         + 1);
        for (i = 0; i < skip; i++) {
            take->slots[take->count++] = i;
            bind(scope, pattern->existentials[i], new_slot(scope));
        }
        for (i = 0; i < pattern->argument_count; i++) {
            if (pattern->arguments[i]->kind == P_WILDCARD) {
                continue;
            }
            take->slots[take->count++] = skip + i;
            fields[count].pattern = pattern->arguments[i];
            fields[count].slot = new_slot(scope);
            fields[count].evaluated = 0;
            fields[count].tag_known = 0;
            count++;
        }
        for (i = 0; i < count; i++) {
            fields[i].next = i + 1 < count ? &fields[i + 1] : rest;
        }
        if (take->count > 0) {
            *last = take;
            last = &take->next;
        }
        *last = compile_patterns(scope, count > 0 ? fields : rest, then, data,
                                 fail);
        free(fields);
        return start;
    }
    case P_RECORD:
        break;
    }
    die("an unresolved pattern reached the compiler");
    return NULL;
}

/* ---- Alternatives ---- */

static Code *compile_guards(Scope *scope, const Guard *guard, Code *fail);

/* What is left of a guard: its qualifiers from `qualifier` on. */
typedef struct Rest {
    const Qualifier *qualifier;
    const Guard *guard;
} Rest;

/* The qualifiers of a guard and its body; `fail` if one does not hold. */
static Code *compile_qualifiers(Scope *scope, void *data, Code *fail)
{
    const Rest *rest = data;
    const Qualifier *qualifier = rest->qualifier;
    Rest after;
    Expr *expr;
    Code *code;
    Code *test;
    int slot;

    if (qualifier == NULL) {
        return compile(scope, rest->guard->body);
    }
    after.qualifier = qualifier->next;
    after.guard = rest->guard;
    if (qualifier->let != NULL) {
        return compile_let(scope, qualifier->let, compile_qualifiers, &after,
                           fail);
    }
    expr = without_annotations(qualifier->expr);
    slot = expr->kind == E_LOCAL ? find_slot(scope, expr->local) : -1;
    if (qualifier->pattern != NULL) {
        Obligation obligation;

        memset(&obligation, 0, sizeof(obligation));
        obligation.pattern = qualifier->pattern;
        if (matches_anything(qualifier->pattern)) {
            /* The value is not evaluated. */
            if (slot >= 0) {
                obligation.slot = slot;
                return compile_patterns(scope, &obligation, compile_qualifiers,
                                        &after, fail);
            }
            code = new_code(scope, C_LET, expr->token);
            code->count = 1;
            code->slots = xmalloc(sizeof(int));
            code->codes = xmalloc(sizeof(Code *));
            code->slots[0] = new_slot(scope);
            code->codes[0] = compile_delayed(scope, expr);
            obligation.slot = code->slots[0];
        } else {
            if (slot < 0) {
                slot = new_slot(scope);
            }
            code = new_eval(scope, compile_scrutinee(scope, expr), slot,
                            expr->token);
            obligation.slot = slot;
            obligation.evaluated = 1;
        }
        code->next = compile_patterns(scope, &obligation, compile_qualifiers,
                                      &after, fail);
        return code;
    }
    if (slot < 0) {
        slot = new_slot(scope);
    }
    code = new_eval(scope, compile_scrutinee(scope, expr), slot, expr->token);
    test = new_code(scope, C_TEST, expr->token);
    test->slot = slot;
    test->tag = TAG_TRUE;
    test->next = fail;
    code->next = test;
    test->first = compile_qualifiers(scope, &after, fail);
    return code;
}

/* The guards from `guard` on: the body of the first one that holds, or
   `fail`. */
static Code *compile_guards(Scope *scope, const Guard *guard, Code *fail)
{
    Rest rest;
    Mark mark;
    Code *code;

    if (guard->next != NULL) {
        fail = compile_guards(scope, guard->next, fail);
    }
    rest.qualifier = guard->qualifiers;
    rest.guard = guard;
    mark = mark_scope(scope);
    code = compile_qualifiers(scope, &rest, fail);
    reset_scope(scope, mark);
    return code;
}

/* What comes after the pattern of an alternative. */
static Code *compile_rhs(Scope *scope, void *data, Code *fail)
{
    const Alternative *alternative = data;

    if (alternative->body != NULL) {
        return compile(scope, alternative->body);
    }
    return compile_guards(scope, alternative->guards, fail);
}

static Code *compile_where(Scope *scope, void *data, Code *fail)
{
    const Alternative *alternative = data;

    if (alternative->where != NULL) {
        return compile_let(scope, alternative->where, compile_rhs, data, fail);
    }
    return compile_rhs(scope, data, fail);
}

/* The values a case looks at: one, or the fields of a constructor that
   the case is given whole (the parameters of a function with several
   clauses come as a tuple). Each alternative has a pattern for each. */
typedef struct Columns {
    int count;
    int *slots;
    int *evaluated;
} Columns;

static const Pattern *column_pattern(const Alternative *alternative,
                                     const Columns *columns, int column,
                                     int whole)
{
    (void)columns;
    return whole ? alternative->pattern
                 : alternative->pattern->arguments[column];
}

/* The first column whose pattern needs the value evaluated, or -1. */
static int lead_column(const Alternative *alternative, const Columns *columns,
                       int whole)
{
    int i;

    for (i = 0; i < columns->count; i++) {
        if (!matches_anything(column_pattern(alternative, columns, i, whole))) {
            return i;
        }
    }
    return -1;
}

/* Whether the alternative starts by asking which constructor the value in
   its lead column is, of a type that has several. */
static int starts_with_switch(const Alternative *alternative,
                              const Columns *columns, int whole)
{
    int lead = lead_column(alternative, columns, whole);
    const Pattern *pattern;

    if (lead < 0) {
        return 0;
    }
    pattern = constructor_pattern(
        column_pattern(alternative, columns, lead, whole));
    return pattern != NULL && sibling_count(pattern->constructor) > 1;
}

/* One alternative: its patterns, then its right-hand side. `known` is the
   column whose constructor a C_SWITCH has already seen, or -1. */
static Code *compile_alternative(Scope *scope, const Alternative *alternative,
                                 const Columns *columns, int whole, int known,
                                 Code *fail)
{
    Obligation *obligations =
        xmalloc((size_t)columns->count * sizeof(Obligation) + 1);
    Mark mark = mark_scope(scope);
    Code *code;
    int i;

    for (i = 0; i < columns->count; i++) {
        obligations[i].pattern = column_pattern(alternative, columns, i, whole);
        obligations[i].slot = columns->slots[i];
        obligations[i].evaluated = columns->evaluated[i] || i == known;
        obligations[i].tag_known = i == known;
        obligations[i].next = i + 1 < columns->count ? &obligations[i + 1]
                                                     : NULL;
    }
    code = compile_patterns(scope, columns->count > 0 ? obligations : NULL,
                            compile_where, (void *)alternative, fail);
    reset_scope(scope, mark);
    free(obligations);
    return code;
}

/* The alternatives in order: the first that matches. A run of
   alternatives that each start by asking which constructor the same value
   is becomes one C_SWITCH on it. An alternative of the run that fails
   further on goes to the next one for the same constructor, or to what
   comes after the run. */
static Code *compile_alternatives(Scope *scope, const Alternative **alternatives,
                                  int count, const Columns *columns, int whole,
                                  Code *fail)
{
    Code *code = fail;
    int i = count - 1;

    while (i >= 0) {
        int lead = lead_column(alternatives[i], columns, whole);
        int first = i;
        const Pattern *pattern;
        Code *choose;
        int k;

        if (!starts_with_switch(alternatives[i], columns, whole)) {
            code = compile_alternative(scope, alternatives[i], columns, whole,
                                       -1, code);
            i--;
            continue;
        }
        while (first > 0
               && starts_with_switch(alternatives[first - 1], columns, whole)
               && lead_column(alternatives[first - 1], columns, whole) == lead) {
            first--;
        }
        pattern = constructor_pattern(
            column_pattern(alternatives[i], columns, lead, whole));
        choose = new_code(scope, C_SWITCH, pattern->token);
        choose->slot = columns->slots[lead];
        choose->count = sibling_count(pattern->constructor);
        choose->codes = xmalloc((size_t)choose->count * sizeof(Code *));
        for (k = 0; k < choose->count; k++) {
            choose->codes[k] = NULL;
        }
        choose->next = code;
        for (k = i; k >= first; k--) {
            int tag;

            pattern = constructor_pattern(
                column_pattern(alternatives[k], columns, lead, whole));
            tag = pattern->constructor->tag;
            choose->codes[tag] = compile_alternative(
                scope, alternatives[k], columns, whole, lead,
                choose->codes[tag] != NULL ? choose->codes[tag] : code);
        }
        code = choose;
        if (!columns->evaluated[lead]) {
            code = new_eval(scope,
                            new_local(scope, choose->slot, pattern->token),
                            choose->slot, pattern->token);
            code->next = choose;
        }
        i = first - 1;
    }
    return code;
}

/* Whether the case is given a constructor with all its fields, and every
   alternative takes it apart again: then it is never made. */
static int is_known_constructor(const Expr *head, int count,
                                const Expr *expr)
{
    const Alternative *alternative;

    if (!is_saturated(head, count) || head->constructor->is_newtype
        || sibling_count(head->constructor) != 1
        || head->constructor->existential_count != 0
        || has_strict_fields(head->constructor)) {
        return 0;
    }
    for (alternative = expr->alternatives; alternative != NULL;
         alternative = alternative->next) {
        if (alternative->pattern->kind != P_CONSTRUCTOR
            || alternative->pattern->constructor != head->constructor) {
            return 0;
        }
    }
    return 1;
}

static Code *compile_case(Scope *scope, Expr *expr)
{
    Expr *scrutinee = without_annotations(expr->scrutinee);
    const Alternative **alternatives;
    const Alternative *alternative;
    Expr **arguments;
    Columns columns;
    Code *start = NULL;
    Code **last = &start;
    Code *fail = new_failure(scope, "no alternative matches", expr->token);
    Expr *head;
    int argument_count;
    int count = 0;
    int whole = 1;
    int i;

    for (alternative = expr->alternatives; alternative != NULL;
         alternative = alternative->next) {
        count++;
    }
    alternatives = xmalloc((size_t)count * sizeof(Alternative *) + 1);
    count = 0;
    for (alternative = expr->alternatives; alternative != NULL;
         alternative = alternative->next) {
        alternatives[count++] = alternative;
    }
    head = split_application(scrutinee, &arguments, &argument_count);
    if (is_known_constructor(head, argument_count, expr)) {
        Code *let = new_code(scope, C_LET, expr->token);

        whole = 0;
        columns.count = argument_count;
        columns.slots = xmalloc((size_t)argument_count * sizeof(int) + 1);
        let->slots = xmalloc((size_t)argument_count * sizeof(int) + 1);
        let->codes = xmalloc((size_t)argument_count * sizeof(Code *) + 1);
        for (i = 0; i < argument_count; i++) {
            Expr *argument = without_annotations(arguments[i]);
            int slot = argument->kind == E_LOCAL
                ? find_slot(scope, argument->local) : -1;

            if (slot < 0) {
                slot = new_slot(scope);
                let->slots[let->count] = slot;
                let->codes[let->count] = compile_delayed(scope, argument);
                let->count++;
            }
            columns.slots[i] = slot;
        }
        if (let->count > 0) {
            *last = let;
            last = &let->next;
        }
    } else {
        int slot = scrutinee->kind == E_LOCAL
            ? find_slot(scope, scrutinee->local) : -1;

        columns.count = 1;
        columns.slots = xmalloc(sizeof(int));
        if (!matches_anything(alternatives[0]->pattern)) {
            /* The first pattern looks at the value. */
            Code *eval;

            if (slot < 0) {
                slot = new_slot(scope);
            }
            eval = new_eval(scope, compile_scrutinee(scope, scrutinee), slot,
                            expr->token);
            *last = eval;
            last = &eval->next;
            whole = 2;
        } else if (slot < 0) {
            /* It does not, so the value is not evaluated: case e of
               _ -> ... */
            Code *let = new_code(scope, C_LET, expr->token);

            slot = new_slot(scope);
            let->count = 1;
            let->slots = xmalloc(sizeof(int));
            let->codes = xmalloc(sizeof(Code *));
            let->slots[0] = slot;
            let->codes[0] = compile_delayed(scope, scrutinee);
            *last = let;
            last = &let->next;
        }
        columns.slots[0] = slot;
    }
    columns.evaluated = xmalloc((size_t)columns.count * sizeof(int) + 1);
    for (i = 0; i < columns.count; i++) {
        columns.evaluated[i] = whole == 2;
    }
    *last = compile_alternatives(scope, alternatives, count, &columns,
                                 whole != 0, fail);
    free(arguments);
    free(alternatives);
    free(columns.slots);
    free(columns.evaluated);
    return start;
}

/* Code that evaluates the expression: the last thing its function does. */
static Code *compile(Scope *scope, Expr *expr)
{
    Code *code;
    int slot;

    switch (expr->kind) {
    case E_LOCAL:
        slot = find_slot(scope, expr->local);
        if (slot < 0) {
            return new_failure(scope, "a variable is not in scope",
                               expr->token);
        }
        return new_local(scope, slot, expr->token);
    case E_GLOBAL:
        code = new_code(scope, C_GLOBAL, expr->token);
        code->u.global = expr->global;
        return code;
    case E_CONSTRUCTOR:
        return compile_constructor(scope, expr->constructor, expr->token);
    case E_INT:
        if (expr->big == 0) {
            return new_const(scope, constant_int(expr->number), expr->token);
        }
        code = new_code(scope, C_INTEGER, expr->token);
        code->tag = expr->big;
        if (expr->big == 2) {
            code->u.string = expr->string;
        } else {
            code->u.number = expr->number;
        }
        return code;
    case E_CHAR:
        return new_const(scope, constant_char(expr->number), expr->token);
    case E_STRING:
        code = new_code(scope, C_STRING, expr->token);
        code->u.string = expr->string;
        return code;
    case E_APP:
        return compile_application(scope, expr);
    case E_LAMBDA:
        return compile_function(scope, expr, 0);
    case E_CASE:
        return compile_case(scope, expr);
    case E_LET:
        return compile_let(scope, expr, let_body, expr->body, NULL);
    case E_ANNOTATION:
        return compile(scope, expr->body);
    case E_TYPE:
        return compile_type(scope, expr);
    case E_METHOD:
        return compile_method(scope, expr);
    case E_NAME:
    case E_OPERATORS:
    case E_RECORD:
    case E_VISIBLE:
        break;
    }
    die("an unresolved expression reached the compiler");
    return NULL;
}

/* ---- What a C_EVAL lets go of ----

   While a C_EVAL waits for its value, the slots of its function stay as
   they are. A slot that the code after it never reads would keep its value
   in use all that time: a parser that has its input in a variable would
   keep all of the input until the parse is over, and a program that runs
   in constant memory would not. So each C_EVAL empties the slots that are
   not read after it. Two passes over the code of a function find them:
   which slots the code from a node on may read (backwards), and which
   slots may hold something when a node is reached (forwards). */

typedef unsigned long Word;
#define WORD_BITS ((int)(sizeof(Word) * 8))

typedef struct Analysis {
    int words;    /* in a set of slots */
    Word **read;  /* by Code.id: the slots read from the node on, or NULL */
    Word **held;  /* by Code.id: the slots that may hold something there */
} Analysis;

static Word *new_set(const Analysis *analysis)
{
    Word *set = xmalloc((size_t)analysis->words * sizeof(Word));

    memset(set, 0, (size_t)analysis->words * sizeof(Word));
    return set;
}

static void add_slot(Word *set, int slot)
{
    set[slot / WORD_BITS] |= (Word)1 << (slot % WORD_BITS);
}

static void remove_slot(Word *set, int slot)
{
    set[slot / WORD_BITS] &= ~((Word)1 << (slot % WORD_BITS));
}

static int has_slot(const Word *set, int slot)
{
    return (set[slot / WORD_BITS] >> (slot % WORD_BITS)) & 1;
}

static void add_set(const Analysis *analysis, Word *set, const Word *other)
{
    int i;

    for (i = 0; i < analysis->words; i++) {
        set[i] |= other[i];
    }
}

/* The slots that the code reads itself, not counting what follows it. */
static void add_reads(Word *set, const Code *code)
{
    int i;

    switch (code->kind) {
    case C_LOCAL:
        add_slot(set, code->slot);
        break;
    case C_CLOSURE:
    case C_ENTER:
    case C_TYPE:
    case C_METHOD:
        for (i = 0; i < code->count; i++) {
            add_slot(set, code->slots[i]);
        }
        break;
    case C_CONSTRUCT:
        for (i = 0; i < code->count; i++) {
            add_reads(set, code->codes[i]);
        }
        break;
    case C_PRIM:
        for (i = 0; i < code->count; i++) {
            add_reads(set, code->codes[i]);
        }
        break;
    case C_CALL:
        add_reads(set, code->first);
        for (i = 0; i < code->count; i++) {
            add_reads(set, code->codes[i]);
        }
        break;
    default:
        break;
    }
}

/* The slots that are read from the node on, before they are written. */
static const Word *slots_read(Analysis *analysis, const Code *code)
{
    Word *set = analysis->read[code->id];
    int i;

    if (set != NULL) {
        return set;
    }
    set = new_set(analysis);
    analysis->read[code->id] = set;
    switch (code->kind) {
    case C_EVAL:
        add_set(analysis, set, slots_read(analysis, code->next));
        remove_slot(set, code->slot);
        add_reads(set, code->first);
        break;
    case C_LET:
        add_set(analysis, set, slots_read(analysis, code->next));
        for (i = 0; i < code->count; i++) {
            add_reads(set, code->codes[i]);
        }
        for (i = 0; i < code->count; i++) {
            remove_slot(set, code->slots[i]);
        }
        break;
    case C_FIELDS:
        add_set(analysis, set, slots_read(analysis, code->next));
        for (i = 0; i < code->count; i++) {
            remove_slot(set, code->tag + i);
        }
        add_slot(set, code->slot);
        break;
    case C_TEST:
    case C_TEST_CHAR:
        add_set(analysis, set, slots_read(analysis, code->first));
        add_set(analysis, set, slots_read(analysis, code->next));
        add_slot(set, code->slot);
        break;
    case C_SWITCH:
        add_set(analysis, set, slots_read(analysis, code->next));
        for (i = 0; i < code->count; i++) {
            if (code->codes[i] != NULL) {
                add_set(analysis, set, slots_read(analysis, code->codes[i]));
            }
        }
        add_slot(set, code->slot);
        break;
    default:
        add_reads(set, code);
        break;
    }
    return set;
}

/* Notes that the slots in `held` may hold something when the node is
   reached, and the same for the nodes after it. A node that several
   others go to is looked at again when that adds something. */
static void note_held(Analysis *analysis, const Code *code, const Word *held)
{
    Word *set = analysis->held[code->id];
    Word *after;
    int changed = 0;
    int i;

    if (set == NULL) {
        set = new_set(analysis);
        analysis->held[code->id] = set;
        changed = 1;
    }
    for (i = 0; i < analysis->words; i++) {
        if ((held[i] & ~set[i]) != 0) {
            set[i] |= held[i];
            changed = 1;
        }
    }
    if (!changed) {
        return;
    }
    after = new_set(analysis);
    add_set(analysis, after, set);
    switch (code->kind) {
    case C_EVAL:
        if (code->first->kind == C_CALL || code->first->kind == C_ENTER) {
            /* It always waits, so what it lets go of is gone after it. */
            const Word *read = slots_read(analysis, code->next);

            for (i = 0; i < analysis->words; i++) {
                after[i] &= read[i];
            }
        }
        add_slot(after, code->slot);
        note_held(analysis, code->next, after);
        break;
    case C_LET:
        for (i = 0; i < code->count; i++) {
            add_slot(after, code->slots[i]);
        }
        note_held(analysis, code->next, after);
        break;
    case C_FIELDS:
        for (i = 0; i < code->count; i++) {
            add_slot(after, code->tag + i);
        }
        note_held(analysis, code->next, after);
        break;
    case C_TEST:
    case C_TEST_CHAR:
        note_held(analysis, code->first, after);
        note_held(analysis, code->next, after);
        break;
    case C_SWITCH:
        note_held(analysis, code->next, after);
        for (i = 0; i < code->count; i++) {
            if (code->codes[i] != NULL) {
                note_held(analysis, code->codes[i], after);
            }
        }
        break;
    default:
        break;
    }
    free(after);
}

/* Gives the arguments of the call the slots to empty when they are
   evaluated before the call: those of `held` that neither the arguments
   after it read nor what comes after the call, which is `needed`. An
   argument that is a call itself (Code.first) is run in place: it lets
   go of the same when it has its own arguments, which get theirs in
   turn. `needed` is changed. */
static void argument_kills(Analysis *analysis, const Function *function,
                           const Code *call, const Word *held, Word *needed)
{
    int slot;
    int k;

    for (k = call->count - 1; k >= 0; k--) {
        Code *argument = call->codes[k];

        if (argument->kind == C_LOCAL || argument->kind == C_CLOSURE) {
            argument->let_go =
                xmalloc((size_t)function->slot_count * sizeof(int));
            for (slot = 0; slot < function->slot_count; slot++) {
                if (has_slot(held, slot) && !has_slot(needed, slot)) {
                    argument->let_go[argument->let_go_count++] = slot;
                }
            }
            argument->let_go = xrealloc(
                argument->let_go,
                (size_t)argument->let_go_count * sizeof(int) + 1);
        }
        if (argument->kind == C_CLOSURE && argument->first != NULL) {
            Word *after = new_set(analysis);

            argument->first->let_go = argument->let_go;
            argument->first->let_go_count = argument->let_go_count;
            add_set(analysis, after, needed);
            argument_kills(analysis, function, argument->first, held, after);
            free(after);
        }
        add_reads(needed, argument);
    }
}

/* Gives every C_EVAL of the function, and every argument of its calls,
   the slots to empty. */
static void find_kills(Scope *scope, Function *function)
{
    Analysis analysis;
    Word *held;
    int i;
    int slot;

    if (function->slot_count == 0) {
        return;
    }
    analysis.words = (function->slot_count + WORD_BITS - 1) / WORD_BITS;
    analysis.read = xmalloc((size_t)scope->node_count * sizeof(Word *) + 1);
    analysis.held = xmalloc((size_t)scope->node_count * sizeof(Word *) + 1);
    for (i = 0; i < scope->node_count; i++) {
        analysis.read[i] = NULL;
        analysis.held[i] = NULL;
    }
    /* On entry: the parameters and the captured variables. */
    held = new_set(&analysis);
    for (slot = 0; slot < function->arity + function->capture_count; slot++) {
        add_slot(held, slot);
    }
    note_held(&analysis, function->body, held);
    free(held);
    for (i = 0; i < scope->node_count; i++) {
        Code *code = scope->nodes[i];
        const Word *read;

        if (code->kind != C_EVAL || analysis.held[i] == NULL) {
            continue;
        }
        read = slots_read(&analysis, code->next);
        code->let_go = xmalloc((size_t)function->slot_count * sizeof(int));
        for (slot = 0; slot < function->slot_count; slot++) {
            if (slot != code->slot && has_slot(analysis.held[i], slot)
                && !has_slot(read, slot)) {
                code->let_go[code->let_go_count++] = slot;
            }
        }
        code->let_go = xrealloc(code->let_go, (size_t)code->let_go_count
                                                  * sizeof(int) + 1);
    }
    /* The arguments of a call: one that is evaluated before the call lets
       go of what neither the arguments after it nor the code after the
       call reads. */
    for (i = 0; i < scope->node_count; i++) {
        Code *code = scope->nodes[i];
        const Code *call = code->kind == C_EVAL ? code->first : code;
        Word *needed;

        if ((call->kind != C_CALL && call->kind != C_PRIM)
            || analysis.held[i] == NULL) {
            continue;
        }
        needed = new_set(&analysis);
        if (code->kind == C_EVAL) {
            add_set(&analysis, needed, slots_read(&analysis, code->next));
        }
        argument_kills(&analysis, function, call, analysis.held[i], needed);
        free(needed);
    }
    for (i = 0; i < scope->node_count; i++) {
        free(analysis.read[i]);
        free(analysis.held[i]);
    }
    free(analysis.read);
    free(analysis.held);
}

/* ---- What a function always evaluates ----

   A function that always evaluates a parameter gets the argument for it
   evaluated: the machine does that before the call, without making a
   thunk first (C_CALL). "Always" is what can be seen in its code without
   looking into other functions: the parameter is evaluated on every way
   through the code, or handed to a builtin that evaluates it. */

typedef struct Strictness {
    const Function *function;
    unsigned *known; /* by Code.id */
    char *done;
} Strictness;

static unsigned parameter_bit(const Strictness *strictness, int slot)
{
    return slot < strictness->function->arity && slot < 32 ? 1u << slot : 0;
}

/* Whether the call gives a builtin that always evaluates some of its
   arguments all of them. Those builtins only work something out: they
   have no effect. */
static int is_pure_builtin(const Code *call)
{
    const Builtin *builtin;

    if (call->kind == C_PRIM) {
        return 1;
    }
    if (call->first->kind != C_GLOBAL || call->first->u.global->builtin == NULL) {
        return 0;
    }
    builtin = find_builtin(call->first->u.global->builtin);
    return builtin->arity > 0 && call->count >= builtin->arity
        && global_value(call->first->u.global)->tag != 0;
}

/* The parameters that a call evaluates before it does anything else. */
static unsigned call_evaluates(const Strictness *strictness, const Code *call)
{
    unsigned set = 0;
    unsigned strict;
    int i;

    if (call->kind == C_PRIM) {
        strict = ~0u;
    } else if (call->first->kind == C_LOCAL) {
        /* It is the function that is called. */
        return parameter_bit(strictness, call->first->slot);
    } else if (!is_pure_builtin(call)) {
        return 0;
    } else {
        strict = (unsigned)global_value(call->first->u.global)->tag;
    }
    for (i = 0; i < call->count && i < 32; i++) {
        if (call->codes[i]->kind == C_LOCAL && ((strict >> i) & 1) != 0) {
            set |= parameter_bit(strictness, call->codes[i]->slot);
        }
    }
    return set;
}

/* The parameters that are evaluated whichever way the code from the node
   on goes. */
static unsigned always_evaluated(Strictness *strictness, const Code *code)
{
    unsigned set = 0;
    int all = 1;
    int i;

    if (strictness->done[code->id]) {
        return strictness->known[code->id];
    }
    switch (code->kind) {
    case C_LOCAL:
        set = parameter_bit(strictness, code->slot);
        break;
    case C_EVAL:
        /* What the code after a call evaluates does not count, unless
           the call is of a builtin that only works something out: a
           call can be of an action, and what comes after an action must
           not be evaluated before it has had its effect. */
        if (code->first->kind == C_LOCAL) {
            set = parameter_bit(strictness, code->first->slot)
                | always_evaluated(strictness, code->next);
        } else if (code->first->kind == C_CONST) {
            set = always_evaluated(strictness, code->next);
        } else if (code->first->kind == C_CALL
                   || code->first->kind == C_PRIM) {
            set = call_evaluates(strictness, code->first);
            if (is_pure_builtin(code->first)) {
                set |= always_evaluated(strictness, code->next);
            }
        }
        break;
    case C_LET:
    case C_FIELDS:
        set = always_evaluated(strictness, code->next);
        break;
    case C_TEST:
    case C_TEST_CHAR:
        set = always_evaluated(strictness, code->first)
            & always_evaluated(strictness, code->next);
        break;
    case C_SWITCH:
        set = ~0u;
        for (i = 0; i < code->count; i++) {
            if (code->codes[i] != NULL) {
                set &= always_evaluated(strictness, code->codes[i]);
            } else {
                all = 0;
            }
        }
        if (!all) {
            set &= always_evaluated(strictness, code->next);
        }
        break;
    case C_FAIL:
        /* It does not come back. */
        set = ~0u;
        break;
    case C_CALL:
    case C_PRIM:
        set = call_evaluates(strictness, code);
        break;
    default:
        break;
    }
    strictness->known[code->id] = set;
    strictness->done[code->id] = 1;
    return set;
}

/* Whether the code from the node on only looks at values: see
   Function.is_leaf. Notes the slots it reads in `*reads`. `budget` bounds
   the nodes that are looked at. */
static int is_leaf_code(const Code *code, int *budget, unsigned *reads)
{
    int i;

    if (--*budget < 0) {
        return 0;
    }
    switch (code->kind) {
    case C_LOCAL:
        *reads |= 1u << code->slot;
        return 1;
    case C_CONST:
        return 1;
    case C_EVAL:
        if (code->first->kind != C_LOCAL) {
            return 0;
        }
        *reads |= 1u << code->first->slot;
        return is_leaf_code(code->next, budget, reads);
    case C_FIELDS:
        *reads |= 1u << code->slot;
        return is_leaf_code(code->next, budget, reads);
    case C_TEST:
    case C_TEST_CHAR:
        *reads |= 1u << code->slot;
        return is_leaf_code(code->first, budget, reads)
            && is_leaf_code(code->next, budget, reads);
    case C_SWITCH:
        *reads |= 1u << code->slot;
        for (i = 0; i < code->count; i++) {
            if (code->codes[i] != NULL
                && !is_leaf_code(code->codes[i], budget, reads)) {
                return 0;
            }
        }
        return is_leaf_code(code->next, budget, reads);
    default:
        return 0;
    }
}

static void find_strictness(const Scope *scope, Function *function)
{
    int budget = 24;

    function->is_leaf = function->capture_count == 0
        && function->slot_count <= LEAF_SLOTS
        && is_leaf_code(function->body, &budget, &function->leaf_reads);
    Strictness strictness;

    if (function->arity == 0) {
        return;
    }
    strictness.function = function;
    strictness.known = xmalloc((size_t)scope->node_count * sizeof(unsigned));
    strictness.done = xmalloc((size_t)scope->node_count);
    memset(strictness.done, 0, (size_t)scope->node_count);
    function->strict = always_evaluated(&strictness, function->body);
    if (function->arity < 32) {
        function->strict &= (1u << function->arity) - 1;
    }
    free(strictness.known);
    free(strictness.done);
}

/* ---- Top-level variables ---- */

Value *compile_global(Global *global)
{
    Expr *expr = global->expr;
    Code *code;
    Value *value;

    if (without_annotations(expr)->kind == E_LAMBDA) {
        code = compile_function(NULL, without_annotations(expr), 0);
        value = code->u.value;
    } else {
        /* A thunk, so that the variable is evaluated at most once. */
        code = compile_function(NULL, expr, 1);
        value = make_thunk(code->u.function);
    }
    free(code);
    return value;
}
