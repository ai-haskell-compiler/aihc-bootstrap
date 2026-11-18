#include "eval.h"

#include <setjmp.h>
#include <string.h>

/* The evaluator is a machine that runs code (see Code in eval.h), with
   stacks of its own instead of a recursive C function. Deep Haskell
   recursion therefore cannot overflow the C stack, and everything the
   machine still needs is where the collector can find it.

   The locals: the slots of the function that is running, after those of
   the functions that wait for it. A function that is called last by
   another (a tail call) takes the place of that one's slots, so a loop
   runs in constant space.

   The frames: what is to be done with a value once it is evaluated. */

enum FrameKind {
    F_APPLY,  /* then apply the result to the `count` locals below `top` */
    F_UPDATE, /* then store the result in the thunk `value` */
    F_RESUME, /* then put the result in the slot of the C_EVAL `code` and
                 go on after it, with the locals from `base` on */
    F_ARGUMENT, /* then go on with the call `code` (a C_CALL, or the C_EVAL
                   it is in) of `value`: the result is its argument
                   number `count` */
    F_STOP    /* then leave run() */
};

typedef struct Frame {
    enum FrameKind kind;
    int top;   /* how many locals there were when the frame was made:
                  those above are no longer needed when it is reached */
    int base;
    int count;
    int tail;  /* F_ARGUMENT: the call is the last thing its function does */
    const Code *code;
    Value *value;
} Frame;

/* A catch_exception that is waiting for its function to return. */
typedef struct Handler {
    jmp_buf jump;
    int stack_size; /* how many frames there were when it started */
    int sp;         /* and how many locals */
    struct Handler *outer;
} Handler;

static Handler *handlers = NULL;
static Value *raised; /* the exception that raise_exception was given */

static Frame *stack = NULL;
static int stack_size = 0;
static int stack_capacity = 0;

static Value **locals = NULL;
static int locals_capacity = 0;
static int sp = 0; /* how many locals are in use */

static Frame *push(enum FrameKind kind, int top)
{
    Frame *frame;

    if (stack_size == stack_capacity) {
        stack_capacity = stack_capacity * 2 + 1024;
        stack = xrealloc(stack, (size_t)stack_capacity * sizeof(Frame));
    }
    frame = &stack[stack_size++];
    frame->kind = kind;
    frame->top = top;
    frame->base = 0;
    frame->count = 0;
    frame->code = NULL;
    frame->value = NULL;
    return frame;
}

/* Makes room for `top` locals. This may move them: nothing keeps a
   pointer into them across it. */
static void reserve(int top)
{
    if (top > locals_capacity) {
        locals_capacity = top * 2 + 1024;
        locals = xrealloc(locals, (size_t)locals_capacity * sizeof(Value *));
    }
}

static Value *new_value(enum ValueKind kind)
{
    Value *value = gc_alloc_value(0);

    value->kind = kind;
    return value;
}

/* Small numbers and characters are made once: the lexer of aihc-parser
   makes a value for every character it reads. They are outside the
   collector's memory, and never change. */
#define SHARED_INT_LOW (-128)
#define SHARED_INT_HIGH 1024
#define SHARED_CHARS 256

static Value shared_ints[SHARED_INT_HIGH - SHARED_INT_LOW];
static Value shared_chars[SHARED_CHARS];

Value *make_unshared_int(long number)
{
    Value *value = new_value(V_INT);

    value->a.number = number;
    return value;
}

Value *make_int(long number)
{
    Value *value;

    if (number < SHARED_INT_LOW || number >= SHARED_INT_HIGH) {
        return make_unshared_int(number);
    }
    value = &shared_ints[number - SHARED_INT_LOW];
    if (value->kind != V_INT || value->a.number != number) {
        value->kind = V_INT;
        value->a.number = number;
    }
    return value;
}

/* A number for code to refer to: not in the collector's memory. */
Value *constant_int(long number)
{
    Value *value;

    if (number >= SHARED_INT_LOW && number < SHARED_INT_HIGH) {
        return make_int(number);
    }
    value = xmalloc(sizeof(Value));
    memset(value, 0, sizeof(Value));
    value->kind = V_INT;
    value->a.number = number;
    return value;
}

Value *constant_char(long code)
{
    Value *value;

    if (code >= 0 && code < SHARED_CHARS) {
        return make_char(code);
    }
    value = xmalloc(sizeof(Value));
    memset(value, 0, sizeof(Value));
    value->kind = V_CHAR;
    value->a.number = code;
    return value;
}

Value *make_char(long code)
{
    Value *value;

    if (code >= 0 && code < SHARED_CHARS) {
        value = &shared_chars[code];
        value->kind = V_CHAR;
        value->a.number = code;
        return value;
    }
    value = new_value(V_CHAR);
    value->a.number = code;
    return value;
}

/* The fields of a constructor that has none. */
static Value *no_fields[1];

/* A constructor without fields is made once for each tag, outside the
   collector's memory, as small numbers are: True, Nothing, (). */
static Value shared_nullary[SHARED_TAGS];

/* The fields are in the cell of the value, right after it: one allocation
   instead of two, and they are next to it in memory. */
Value *make_con(int tag, int field_count)
{
    Value *value;

    if (field_count == 0 && tag >= 0 && tag < SHARED_TAGS) {
        value = &shared_nullary[tag];
        if (value->kind != V_CON) {
            value->kind = V_CON;
            value->tag = tag;
            value->is_deep = 1;
            value->b.fields = no_fields;
        }
        return value;
    }
    value = gc_alloc_value(field_count);

    value->kind = V_CON;
    value->tag = tag;
    value->a.field_count = field_count;
    value->b.fields = field_count > 0 ? (Value **)(value + 1) : no_fields;
    return value;
}

/* A closure or thunk of the function, with room for what it captures. */
static Value *new_closure(const Function *function)
{
    Value *value = gc_alloc_value(function->capture_count);

    value->kind = function->arity == 0 ? V_THUNK : V_CLOSURE;
    value->a.function = function;
    value->b.fields = (Value **)(value + 1);
    return value;
}

Value *make_thunk(const Function *function)
{
    return new_closure(function);
}

/* Thunks that code refers to: see shared_thunk. */
static Value **shared_thunks = NULL;
static int shared_thunk_count = 0;
static int shared_thunk_capacity = 0;

/* A thunk for code to refer to, so that what it stands for is worked out
   once however often the code is run. The collector is told about it for
   good. */
Value *shared_thunk(const Function *function)
{
    Value *value = new_closure(function);

    if (shared_thunk_count == shared_thunk_capacity) {
        shared_thunk_capacity = shared_thunk_capacity * 2 + 1024;
        shared_thunks = xrealloc(shared_thunks, (size_t)shared_thunk_capacity
                                                    * sizeof(Value *));
    }
    shared_thunks[shared_thunk_count++] = value;
    return value;
}

/* A function that captures nothing never changes and is made once: not
   in the collector's memory. */
Value *constant_closure(const Function *function)
{
    Value *value = xmalloc(sizeof(Value));

    memset(value, 0, sizeof(Value));
    value->kind = V_CLOSURE;
    value->a.function = function;
    value->b.fields = no_fields;
    return value;
}

/* Which arguments the builtin always evaluates: a bit for each. A thunk
   that the builtin would evaluate at once is not worth making. */
static int strict_arguments(const Builtin *builtin)
{
    static const char *const all[] = {
        "bytesLength", "bytesIndex", "bytesSlice", "bytesCompare", NULL
    };
    int i;

    if (strncmp(builtin->name, "num", 3) == 0) {
        return ~0;
    }
    if (strcmp(builtin->name, "seq") == 0) {
        /* seq :: a -> b -> b, after the two types: the first value. */
        return 4;
    }
    for (i = 0; all[i] != NULL; i++) {
        if (strcmp(builtin->name, all[i]) == 0) {
            return ~0;
        }
    }
    return 0;
}

Value *make_builtin(const Builtin *builtin)
{
    Value *value = new_value(V_BUILTIN);

    value->c.builtin = builtin;
    value->tag = strict_arguments(builtin);
    return value;
}

Value *make_io_result(Value *result)
{
    Value *box = make_con(0, 1);

    box->b.fields[0] = result;
    return box;
}

/* Types are never freed, and a type has no variables in it at run time, so
   each has one value, outside the collector's memory. */
Value *make_type(Type *type)
{
    Value *value = type->value;

    if (value == NULL) {
        value = xmalloc(sizeof(Value));
        memset(value, 0, sizeof(Value));
        value->kind = V_TYPE;
        value->b.type = type;
        type->value = value;
    }
    return value;
}

/* The string as a list of characters. */
Value *make_string(const char *string)
{
    Value *list = make_con(TAG_NIL, 0);
    Value **last = &list;

    while (*string != '\0') {
        Value *cell = make_con(TAG_CONS, 2);

        cell->b.fields[0] = make_char(utf8_decode(&string));
        cell->b.fields[1] = *last;
        *last = cell;
        last = &cell->b.fields[1];
    }
    return list;
}

/* ---- Types and methods ---- */

/* The type of a C_TYPE or C_METHOD with its variables replaced by the
   types they stand for, which are in slots. */
static Type *close_type(Type *type, const Code *code, int base)
{
    int i;

    type = prune(type);
    switch (type->kind) {
    case TY_RIGID:
        for (i = 0; i < code->count; i++) {
            if (code->bindings[i] == type->variable) {
                return force(locals[base + code->slots[i]])->b.type;
            }
        }
        die("type variable %s is not in scope", type->variable->name);
        break;
    case TY_APP: {
        /* Types made here are never freed, so one without variables in it
           is not made again. */
        Type *function = close_type(type->function, code, base);
        Type *argument = close_type(type->argument, code, base);

        if (function == type->function && argument == type->argument) {
            return type;
        }
        return new_type_application(function, argument);
    }
    case TY_META:
    case TY_CON:
    case TY_FORALL:
        break;
    }
    return type;
}

/* The method of a C_METHOD. What was found the last time is kept, as
   long as no instances come: code mostly runs at one type, also where
   the type is a variable. A type without variables in it is made once,
   so the same type is the same pointer. find_method keeps the value in
   use meanwhile. */
static Value *method_of(const Code *code, int base)
{
    Code *site = (Code *)code;
    Type *type = code->count > 0 ? close_type(code->type, code, base)
                                 : code->type;
    Value *method;

    if (code->method != NULL && code->method_type == type
        && code->method_instances == all_instances) {
        return code->method;
    }
    method = find_method(code->u.global, type);
    if (method_is_remembered(type)) {
        site->method = method;
        site->method_type = type;
        site->method_instances = all_instances;
    }
    return method;
}

/* ---- Values that take no work ---- */

/* The value of a constructor that has not been given any fields yet. */
Value *make_constructor(const Constructor *constructor)
{
    Value *value;

    if (constructor->arity + constructor->existential_count == 0) {
        return make_con(constructor->tag, 0);
    }
    /* It never changes: giving it a field makes a new value. So it is made
       once, outside the collector's memory, as a type's value is. */
    if (constructor->unapplied == NULL) {
        value = xmalloc(sizeof(Value));
        memset(value, 0, sizeof(Value));
        value->kind = V_PARTIAL_CON;
        value->c.constructor = constructor;
        ((Constructor *)constructor)->unapplied = value;
    }
    return constructor->unapplied;
}

Value *constant_constructor(const Constructor *constructor)
{
    if (constructor->arity + constructor->existential_count == 0
        && constructor->tag >= SHARED_TAGS) {
        return NULL;
    }
    return make_constructor(constructor);
}

/* A thunk for a top-level variable that has no value yet. It is worked
   out when the thunk is evaluated, not here: code that is never run
   mentions variables too, and their definitions would all be prepared. */
static Value *delay_global(Global *global)
{
    if (global->builtin != NULL) {
        return global_value(global);
    }
    if (global->stub == NULL) {
        Code *code = xmalloc(sizeof(Code));

        memset(code, 0, sizeof(Code));
        code->kind = C_GLOBAL;
        code->u.global = global;
        global->stub = xmalloc(sizeof(Function));
        memset(global->stub, 0, sizeof(Function));
        global->stub->body = code;
    }
    return make_thunk(global->stub);
}

static Value *delay(const Code *code, int base);

/* A closure or thunk with the values it captures. */
static Value *make_closure(const Code *code, int base)
{
    Value *value = new_closure(code->u.function);
    int i;

    for (i = 0; i < code->count; i++) {
        value->b.fields[i] = locals[base + code->slots[i]];
    }
    return value;
}

/* A constructor with its fields. They are worked out into locals first:
   making one of them may collect, and then the constructor would be found
   half made. */
static Value *construct(const Code *code, int base)
{
    Value *value;
    int first = sp;
    int i;

    reserve(sp + code->count);
    for (i = 0; i < code->count; i++) {
        Value *field = delay(code->codes[i], base);

        locals[sp++] = field;
    }
    value = make_con(code->tag, code->count);
    for (i = 0; i < code->count; i++) {
        value->b.fields[i] = locals[first + i];
    }
    sp = first;
    return value;
}

/* The value of code that takes no work: see CodeKind. Nothing is
   evaluated. */
static Value *delay(const Code *code, int base)
{
    switch (code->kind) {
    case C_LOCAL:
        return locals[base + code->slot];
    case C_CONST:
        return code->u.value;
    case C_GLOBAL:
        return code->u.global->value != NULL ? code->u.global->value
                                             : delay_global(code->u.global);
    case C_CLOSURE:
        return make_closure(code, base);
    case C_CONSTRUCT:
        return construct(code, base);
    case C_TYPE:
        return make_type(close_type(code->type, code, base));
    default:
        break;
    }
    die("code that takes work where a value was expected");
    return NULL;
}

/* ---- Applying ---- */

/* How many arguments a builtin or a constructor takes in all. */
static int arity_of(const Value *function)
{
    if (function->kind == V_BUILTIN) {
        return function->c.builtin->arity;
    }
    return function->c.constructor->arity
           + function->c.constructor->existential_count;
}

/* How many more arguments the function takes before it runs. */
static int arguments_needed(const Value *function)
{
    switch (function->kind) {
    case V_CLOSURE:
        return function->a.function->arity;
    case V_PARTIAL:
        return function->c.function->a.function->arity
               - function->a.argument_count;
    case V_BUILTIN:
    case V_PARTIAL_CON:
        return arity_of(function) - function->a.argument_count;
    default:
        break;
    }
    die("applying something that is not a function");
    return 0;
}

/* Which of the `count` arguments the function is about to be given it
   always evaluates: a bit for each, the first lowest. Only when they are
   all it needs: with fewer it does not run. */
static unsigned strictness(const Value *function, int count)
{
    int had;

    switch (function->kind) {
    case V_CLOSURE:
        return count >= function->a.function->arity
            ? function->a.function->strict : 0;
    case V_PARTIAL:
        had = function->a.argument_count;
        return had + count >= function->c.function->a.function->arity
            ? function->c.function->a.function->strict >> had : 0;
    case V_BUILTIN:
        return function->a.argument_count + count
                >= function->c.builtin->arity
            ? (unsigned)function->tag : 0;
    default:
        return 0;
    }
}

/* The function given the `count` locals from `first` on, which are fewer
   arguments than it needs: a value that waits for the rest. */
static Value *partial_application(Value *function, int first, int count)
{
    int had = function->kind == V_CLOSURE ? 0 : function->a.argument_count;
    Value *result = gc_alloc_value(had + count);
    int i;

    result->kind = function->kind == V_CLOSURE ? V_PARTIAL : function->kind;
    result->a.argument_count = had + count;
    result->b.arguments = (Value **)(result + 1);
    if (function->kind == V_CLOSURE) {
        result->c.function = function;
    } else {
        result->c = function->c;
        /* See Value.tag. */
        result->tag = (int)((unsigned)function->tag >> count);
    }
    for (i = 0; i < had; i++) {
        result->b.arguments[i] = function->b.arguments[i];
    }
    for (i = 0; i < count; i++) {
        result->b.arguments[had + i] = locals[first + i];
    }
    return result;
}

#define BUILTIN_ARGUMENTS 10 /* more than any builtin takes */

/* A builtin or a constructor given the `count` locals from `first` on,
   which are the last arguments it needs: calls the builtin, or makes the
   constructor. */
static Value *complete(Value *function, int first, int count)
{
    int had = function->a.argument_count;
    int total = had + count;
    /* A builtin gets its arguments on the C stack: no builtin keeps the
       array, and the locals may move while it runs. */
    Value *on_c_stack[BUILTIN_ARGUMENTS];
    Value **arguments;
    Value *constructed = NULL;
    int i;

    if (function->kind == V_PARTIAL_CON) {
        const Constructor *constructor = function->c.constructor;

        if (constructor->is_newtype) {
            return locals[first + count - 1];
        }
        constructed = make_con(constructor->tag, total);
        arguments = constructed->b.fields;
    } else if (total <= BUILTIN_ARGUMENTS) {
        arguments = on_c_stack;
    } else {
        arguments = gc_alloc((size_t)total * sizeof(Value *));
    }
    for (i = 0; i < had; i++) {
        arguments[i] = function->b.arguments[i];
    }
    for (i = 0; i < count; i++) {
        arguments[had + i] = locals[first + i];
    }
    if (constructed != NULL) {
        const Constructor *constructor = function->c.constructor;

        for (i = 0; constructor->field_strict != NULL
                    && i < constructor->arity; i++) {
            if (constructor->field_strict[i]) {
                force(arguments[constructor->existential_count + i]);
            }
        }
        return constructed;
    }
    return function->c.builtin->function(arguments);
}

/* ---- The machine ---- */

#define SLOT(index) (locals[base + (index)])

/* Empties the slots that a C_EVAL (or an argument that is evaluated) says
   are not used after it. */
#define LET_GO(eval) \
    do { \
        for (i = 0; i < (eval)->let_go_count; i++) { \
            SLOT((eval)->let_go[i]) = NULL; \
        } \
    } while (0)

/* Whether argument number `index` is one that the function always
   evaluates: see strictness. */
#define IS_STRICT(strict, index) \
    ((index) < 32 && (((strict) >> (index)) & 1) != 0)

/* The machine is in one of two states:
     - code != NULL: running `code`, whose slots are the locals from `base`
       on; `sp` is where those slots end;
     - code == NULL: holding `value`, the result of what was run last.
   It runs until it holds a fully evaluated value and the top frame is
   F_STOP. */
static Value *run(const Code *code, int base, Value *value)
{
    /* A call that is being made: the function, its `argc` arguments in
       the locals from `at` on, and where the locals of the function are
       to start. */
    Value *function = NULL;
    int argc = 0;
    int at = 0;
    int target = 0;
    const Function *callee = NULL;
    const Code *call = NULL;
    const Code *eval = NULL;
    const Code *argument;
    Value *given;
    unsigned strict = 0;
    int index = 0;
    int tail = 0; /* the call is the last thing the function does */
    Frame *frame;
    int need;
    int i;

    for (;;) {
        if (code == NULL) {
            switch (value->kind) {
            case V_THUNK:
                /* Its code runs above the locals that the top frame still
                   needs. */
                callee = value->a.function;
                if (callee->body == NULL) {
                    compile_body((Function *)callee);
                }
                target = stack[stack_size - 1].top;
                reserve(target + callee->slot_count);
                for (i = 0; i < callee->capture_count; i++) {
                    locals[target + i] = value->b.fields[i];
                }
                if (stack[stack_size - 1].kind == F_UPDATE) {
                    /* The thunk on top of the stack is waiting for exactly
                       this value, so this thunk shares its frame: it
                       becomes that thunk. A loop that ends in a `seq` b,
                       where b is a thunk, would otherwise leave a frame
                       for every turn. */
                    value->kind = V_INDIRECTION;
                    value->b.result = stack[stack_size - 1].value;
                    gc_written_value(value);
                } else {
                    frame = push(F_UPDATE, target);
                    frame->value = value;
                    value->kind = V_BLACKHOLE;
                }
                /* A thunk that is being evaluated lets go of what it
                   captured: the machine has that now, and lets go of each
                   part when it is done with it. */
                value->a.function = NULL;
                base = target;
                goto start;
            case V_INDIRECTION:
                value = value->b.result;
                continue;
            case V_BLACKHOLE:
                die("<<loop>>");
                break;
            case V_RAISE:
                raise_exception(value->b.result);
                break;
            default:
                break;
            }
            /* A finished value: give it to the frame that is waiting. */
            frame = &stack[--stack_size];
            switch (frame->kind) {
            case F_STOP:
                sp = frame->top;
                return value;
            case F_UPDATE:
                frame->value->kind = V_INDIRECTION;
                frame->value->b.result = value;
                gc_written_value(frame->value);
                continue;
            case F_RESUME:
                base = frame->base;
                sp = frame->top;
                code = frame->code;
                SLOT(code->slot) = value;
                code = code->next;
                continue;
            case F_APPLY:
                function = value;
                argc = frame->count;
                sp = frame->top;
                at = sp - argc;
                target = at;
                goto apply;
            case F_ARGUMENT:
                /* The arguments before this one are the locals on top. */
                base = frame->base;
                sp = frame->top;
                function = frame->value;
                index = frame->count;
                if (frame->code->kind == C_EVAL) {
                    eval = frame->code;
                    call = eval->first;
                } else {
                    eval = NULL;
                    call = frame->code;
                }
                tail = frame->tail;
                at = sp - index;
                locals[sp++] = value;
                strict = function == NULL ? ~0u
                                          : strictness(function, call->count);
                index++;
                code = call;
                goto arguments;
            }
        }
        switch (code->kind) {
        case C_LOCAL:
            value = SLOT(code->slot);
            break;
        case C_CONST:
            value = code->u.value;
            break;
        case C_GLOBAL:
            value = code->u.global->value != NULL
                ? code->u.global->value : global_value(code->u.global);
            break;
        case C_CLOSURE:
            value = make_closure(code, base);
            break;
        case C_CONSTRUCT:
            value = construct(code, base);
            break;
        case C_TYPE:
            value = make_type(close_type(code->type, code, base));
            break;
        case C_METHOD:
            value = method_of(code, base);
            break;
        case C_STRING:
            value = make_string(code->u.string);
            break;
        case C_INTEGER:
            /* A literal is an Integer. */
            value = code->tag == 2
                ? integer_from_literal(code->u.string)
                : integer_from_unsigned((unsigned long)code->u.number);
            break;
        case C_LET:
            /* A definition may capture any of the bindings, so the
               closures and thunks are all made before any of them is
               given what it captures. */
            for (i = 0; i < code->count; i++) {
                if (code->codes[i]->kind == C_CLOSURE) {
                    Value *made = new_closure(code->codes[i]->u.function);

                    SLOT(code->slots[i]) = made;
                }
            }
            for (i = 0; i < code->count; i++) {
                if (code->codes[i]->kind != C_CLOSURE) {
                    Value *made = delay(code->codes[i], base);

                    SLOT(code->slots[i]) = made;
                }
            }
            for (i = 0; i < code->count; i++) {
                const Code *closure = code->codes[i];
                int k;

                if (closure->kind == C_CLOSURE) {
                    Value *made = SLOT(code->slots[i]);

                    for (k = 0; k < closure->count; k++) {
                        made->b.fields[k] = SLOT(closure->slots[k]);
                    }
                    /* A collection in between may have made it old. */
                    gc_written_value(made);
                }
            }
            code = code->next;
            continue;
        case C_FIELDS: {
            Value **fields = SLOT(code->slot)->b.fields;

            for (i = 0; i < code->count; i++) {
                SLOT(code->tag + i) = fields[code->slots[i]];
            }
            code = code->next;
            continue;
        }
        case C_TEST:
            code = SLOT(code->slot)->tag == code->tag ? code->first
                                                      : code->next;
            continue;
        case C_TEST_CHAR:
            code = SLOT(code->slot)->a.number == code->tag ? code->first
                                                           : code->next;
            continue;
        case C_SWITCH: {
            const Code *chosen = code->codes[SLOT(code->slot)->tag];

            code = chosen != NULL ? chosen : code->next;
            continue;
        }
        case C_FAIL:
            die("%s:%d: %s", code->token->file, code->token->line,
                code->u.string);
            break;
        case C_CALL:
        case C_PRIM:
            /* The last thing the function does: the function that is
               called takes its place. */
            call = code;
            eval = NULL;
            tail = 1;
            goto call;
        case C_EVAL:
            eval = code;
            tail = 0;
            switch (code->first->kind) {
            case C_CALL:
            case C_PRIM:
                call = code->first;
                goto call;
            case C_ENTER:
                callee = code->first->u.function;
                if (callee->body == NULL) {
                    compile_body((Function *)callee);
                }
                target = sp;
                reserve(target + callee->slot_count);
                for (i = 0; i < callee->capture_count; i++) {
                    locals[target + i] = SLOT(code->first->slots[i]);
                }
                LET_GO(code);
                frame = push(F_RESUME, target);
                frame->base = base;
                frame->code = code;
                base = target;
                goto start;
            case C_LOCAL:
                value = SLOT(code->first->slot);
                break;
            case C_CONST:
                value = code->first->u.value;
                break;
            case C_GLOBAL:
                value = code->first->u.global->value != NULL
                    ? code->first->u.global->value
                    : global_value(code->first->u.global);
                break;
            case C_METHOD:
                value = method_of(code->first, base);
                break;
            default:
                die("code that cannot be evaluated in place");
            }
            while (value->kind == V_INDIRECTION) {
                value = value->b.result;
            }
            if (value->kind < V_THUNK) {
                /* Evaluated already, as most values are. */
                SLOT(code->slot) = value;
                code = code->next;
                continue;
            }
            LET_GO(code);
            frame = push(F_RESUME, sp);
            frame->base = base;
            frame->code = code;
            break;
        default:
            die("code of an unknown kind");
        }
        code = NULL;
        continue;

    call:
        /* The arguments go above the locals that are in use. */
        at = sp;
        reserve(at + call->count);
        if (call->kind == C_PRIM) {
            function = NULL;
            strict = ~0u;
            index = 0;
            goto arguments;
        }
        switch (call->first->kind) {
        case C_LOCAL:
            function = SLOT(call->first->slot);
            break;
        case C_CONST:
            function = call->first->u.value;
            break;
        case C_GLOBAL:
            function = call->first->u.global->value != NULL
                ? call->first->u.global->value
                : global_value(call->first->u.global);
            break;
        case C_METHOD:
            function = method_of(call->first, base);
            break;
        default:
            die("code that cannot be called");
        }
        while (function->kind == V_INDIRECTION) {
            function = function->b.result;
        }
        strict = strictness(function, call->count);
        index = 0;
        /* fall through */

    arguments:
        /* The arguments from number `index` on. One that the function
           always evaluates is evaluated here, where no thunk has to be
           made for it: the call goes on when its value is there
           (F_ARGUMENT). */
        for (; index < call->count; index++) {
            argument = call->codes[index];
            if (argument->kind == C_LOCAL) {
                given = SLOT(argument->slot);
            } else if (argument->kind == C_CONST) {
                given = argument->u.value;
            } else if (argument->kind == C_CLOSURE
                       && argument->u.function->arity == 0
                       && IS_STRICT(strict, index)) {
                if (argument->first != NULL) {
                    /* What would be a thunk of a call: the call is made
                       here, and its result is the argument. */
                    frame = push(F_ARGUMENT, sp);
                    frame->base = base;
                    frame->code = eval != NULL ? eval : call;
                    frame->count = index;
                    frame->value = function;
                    frame->tail = tail;
                    call = argument->first;
                    eval = NULL;
                    tail = 0;
                    goto call;
                }
                /* What would be a thunk is run on the spot. */
                callee = argument->u.function;
                if (callee->body == NULL) {
                    compile_body((Function *)callee);
                }
                target = sp;
                reserve(target + callee->slot_count);
                for (i = 0; i < callee->capture_count; i++) {
                    locals[target + i] = SLOT(argument->slots[i]);
                }
                LET_GO(argument);
                frame = push(F_ARGUMENT, target);
                frame->base = base;
                frame->code = eval != NULL ? eval : call;
                frame->count = index;
                frame->value = function;
                frame->tail = tail;
                base = target;
                goto start;
            } else {
                given = delay(argument, base);
            }
            if (given->kind >= V_THUNK && IS_STRICT(strict, index)) {
                while (given->kind == V_INDIRECTION) {
                    given = given->b.result;
                }
                if (given->kind >= V_THUNK) {
                    LET_GO(argument);
                    frame = push(F_ARGUMENT, sp);
                    frame->base = base;
                    frame->code = eval != NULL ? eval : call;
                    frame->count = index;
                    frame->value = function;
                    frame->tail = tail;
                    value = given;
                    code = NULL;
                    break;
                }
            }
            locals[sp++] = given;
        }
        if (index < call->count) {
            continue;
        }
        if (function == NULL) {
            /* A C_PRIM: both numbers are there. */
            value = number_primitive(call->tag, call->slot, locals[at],
                                     locals[at + 1]);
            sp = at;
            if (eval != NULL) {
                LET_GO(eval);
                SLOT(eval->slot) = value;
                code = eval->next;
            } else {
                code = NULL;
            }
            continue;
        }
        argc = call->count;
        if (eval != NULL) {
            /* The function that is running waits for the result. */
            LET_GO(eval);
            frame = push(F_RESUME, at);
            frame->base = base;
            frame->code = eval;
            target = at;
        } else if (tail) {
            target = base;
        } else {
            /* An argument that is run in place: its result goes to the
               call it is an argument of (F_ARGUMENT). */
            LET_GO(call);
            target = at;
        }
        /* fall through */

    apply:
        while (function->kind == V_INDIRECTION) {
            function = function->b.result;
        }
        if (function->kind >= V_THUNK) {
            /* The function is still to be evaluated: the arguments wait. */
            if (at != target) {
                memmove(locals + target, locals + at,
                        (size_t)argc * sizeof(Value *));
            }
            frame = push(F_APPLY, target + argc);
            frame->count = argc;
            sp = target + argc;
            value = function;
            code = NULL;
            continue;
        }
        need = arguments_needed(function);
        if (argc < need) {
            value = partial_application(function, at, argc);
            code = NULL;
            continue;
        }
        if (argc > need) {
            /* More arguments than the function takes: the others wait for
               what it returns. */
            int extra = argc - need;

            reserve(at + argc + need);
            memcpy(locals + at + argc, locals + at,
                   (size_t)need * sizeof(Value *));
            memmove(locals + target, locals + at + need,
                    (size_t)extra * sizeof(Value *));
            frame = push(F_APPLY, target + extra);
            frame->count = extra;
            at += argc;
            target += extra;
            argc = need;
            sp = at + argc;
        }
        switch (function->kind) {
        case V_PARTIAL: {
            /* The arguments it has come before the new ones. */
            int had = function->a.argument_count;

            reserve(target + had + argc);
            memmove(locals + target + had, locals + at,
                    (size_t)argc * sizeof(Value *));
            for (i = 0; i < had; i++) {
                locals[target + i] = function->b.arguments[i];
            }
            at = target;
            argc += had;
            function = function->c.function;
        }
            /* fall through */
        case V_CLOSURE:
            callee = function->a.function;
            reserve(target + callee->slot_count);
            if (at != target) {
                /* A few pointers, moved down: a call of memmove costs
                   more than moving them. */
                Value **to = locals + target;
                Value **from = locals + at;

                if (argc <= 4) {
                    Value *first = from[0];
                    Value *second = argc > 1 ? from[1] : NULL;
                    Value *third = argc > 2 ? from[2] : NULL;
                    Value *fourth = argc > 3 ? from[3] : NULL;

                    to[0] = first;
                    if (argc > 1) {
                        to[1] = second;
                    }
                    if (argc > 2) {
                        to[2] = third;
                    }
                    if (argc > 3) {
                        to[3] = fourth;
                    }
                } else {
                    memmove(to, from, (size_t)argc * sizeof(Value *));
                }
            }
            for (i = 0; i < callee->capture_count; i++) {
                locals[target + callee->arity + i] = function->b.fields[i];
            }
            base = target;
            goto start;
        default:
            value = complete(function, at, argc);
            code = NULL;
            continue;
        }

    start:
        /* The locals of `callee` start at `base`, with its parameters and
           what it captured in place. */
        {
            /* Mostly a slot or two: again not worth a call. */
            Value **slot = locals + base + callee->arity
                           + callee->capture_count;
            int count = callee->slot_count - callee->arity
                        - callee->capture_count;

            if (count <= 3) {
                if (count > 0) {
                    slot[0] = NULL;
                }
                if (count > 1) {
                    slot[1] = NULL;
                }
                if (count > 2) {
                    slot[2] = NULL;
                }
            } else {
                memset(slot, 0, (size_t)count * sizeof(Value *));
            }
        }
        sp = base + callee->slot_count;
        code = callee->body;
    }
}

Value *force(Value *value)
{
    /* Most values are evaluated already: the machine is not needed. */
    while (value->kind == V_INDIRECTION) {
        value = value->b.result;
    }
    if (value->kind < V_THUNK) {
        return value;
    }
    push(F_STOP, sp);
    return run(NULL, 0, value);
}

Value *apply(Value *function, Value *argument)
{
    Frame *frame;

    push(F_STOP, sp);
    reserve(sp + 1);
    locals[sp++] = argument;
    frame = push(F_APPLY, sp);
    frame->count = 1;
    return run(NULL, 0, function);
}

/* What the machine holds on to between allocations, for the collector:
   its locals and frames, the thunks that code refers to, an exception on
   its way to a handler, and the values of the top-level variables. */
void mark_roots(void)
{
    const Global *global;

    gc_mark_range(locals, locals + sp);
    gc_mark_range(stack, stack + stack_size);
    gc_mark_range(shared_thunks, shared_thunks + shared_thunk_count);
    gc_mark(raised);
    mark_remembered_methods();
    for (global = all_globals; global != NULL; global = global->all_next) {
        gc_mark(global->value);
    }
}

/* Prints the lines of the program that the innermost frames of the machine
   are waiting at, the innermost first, each file once. */
void print_position(void)
{
    const char *last = NULL;
    int shown = 0;
    int i;

    fprintf(stderr, "  at");
    for (i = stack_size - 1; i >= 0 && shown < 8; i--) {
        const Code *code = stack[i].code;

        if (code != NULL && code->token != NULL && code->token->file != last) {
            last = code->token->file;
            fprintf(stderr, " %s:%d", last, code->token->line);
            shown++;
        }
    }
    fprintf(stderr, "\n");
}

/* ---- Exceptions ---- */

Value *catch_exception(Value *function, Value *argument, Value **exception)
{
    Handler handler;
    Value *result;

    handler.stack_size = stack_size;
    handler.sp = sp;
    handler.outer = handlers;
    handlers = &handler;
    if (setjmp(handler.jump) == 0) {
        result = apply(function, argument);
        handlers = handler.outer;
        return result;
    }
    /* raise_exception jumped here. Drop what the machine was doing. A thunk
       that was being evaluated raises the same exception when it is
       evaluated again, as in GHC: what it would need to start again is
       gone (see run). */
    handlers = handler.outer;
    while (stack_size > handler.stack_size) {
        stack_size--;
        if (stack[stack_size].kind == F_UPDATE) {
            stack[stack_size].value->kind = V_RAISE;
            stack[stack_size].value->b.result = raised;
            gc_written_value(stack[stack_size].value);
        }
    }
    sp = handler.sp;
    *exception = raised;
    return NULL;
}

void raise_exception(Value *exception)
{
    if (handlers == NULL) {
        die("exception outside of any handler");
    }
    raised = exception;
    longjmp(handlers->jump, 1);
}
