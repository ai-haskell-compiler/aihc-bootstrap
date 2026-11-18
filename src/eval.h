#ifndef EVAL_H
#define EVAL_H

#include <stdio.h>
#include "syntax.h"

typedef struct Value Value;
typedef struct Builtin Builtin;
typedef struct Code Code;
typedef struct Function Function;

enum ValueKind {
    V_INT,
    V_CHAR,
    V_CON,         /* a data constructor applied to all its fields */
    V_CLOSURE,     /* a function together with the variables it uses */
    V_PARTIAL,     /* a V_CLOSURE applied to fewer arguments than it takes */
    V_BUILTIN,     /* a C function applied to fewer arguments than it needs */
    V_PARTIAL_CON, /* a constructor applied to fewer fields than it has */
    V_TYPE,        /* a type, passed to a polymorphic function */
    V_BYTES,       /* a ByteString: `number` bytes at `bytes` (bytes.c) */
    V_THUNK,       /* code that has not been run yet, with the variables it
                      uses */
    V_BLACKHOLE,   /* a thunk that is being evaluated right now */
    V_INDIRECTION, /* a thunk that has been evaluated; see `result` */
    V_RAISE        /* a thunk whose evaluation raised the exception
                      `result`: evaluating it raises that again */
};

/* Every kind uses only some of the fields. A value is made often, so it is
   small: the fields are in three slots, and the kinds that use a slot share
   it. No kind uses two names in one slot. The fields of a constructor, the
   variables of a closure or a thunk and the arguments of a partial
   application are in the same cell, right after the value. Everything is
   boxed: there are no unboxed values anywhere in the interpreter. */
struct Value {
    unsigned char kind;     /* an enum ValueKind */
    unsigned char is_deep;  /* V_CON: every field is evaluated, all the way
                               down (deepForce). A builtin that changes a
                               field takes it away. */
    unsigned char old;      /* the collector has made it old: see
                               gc_written_value */
    int tag;                /* V_CON: index of the constructor in its type */
    union {
        long number;            /* V_INT, V_CHAR, V_BYTES */
        int field_count;        /* V_CON */
        int argument_count;     /* V_PARTIAL, V_BUILTIN, V_PARTIAL_CON */
        const Function *function; /* V_CLOSURE; V_THUNK: NULL once it is
                                     being evaluated */
    } a;
    union {
        Value **fields;         /* V_CON; V_CLOSURE, V_THUNK: the values of
                                   the variables the function captures */
        Value **arguments;      /* V_PARTIAL, V_BUILTIN, V_PARTIAL_CON */
        Value *result;          /* V_INDIRECTION, V_RAISE */
        Type *type;             /* V_TYPE: has no TY_RIGID in it */
        const unsigned char *bytes; /* V_BYTES: `number` bytes (bytes.c) */
    } b;
    union {
        Value *function;        /* V_PARTIAL: the V_CLOSURE */
        const Builtin *builtin; /* V_BUILTIN */
        const Constructor *constructor; /* V_PARTIAL_CON */
    } c;
};

/* ---- Code ----

   A definition is not run as the parser left it. The first time it is
   needed it is compiled (compile.c) into code for the machine (eval.c):

     - every variable is a slot: a place in the locals of the function that
       is running. Nothing is looked up by name;
     - a function takes all its parameters at once, and knows which
       variables of the functions around it it uses: a closure or a thunk
       is made with just their values (captured);
     - patterns are tests on one value at a time, with the code to go to
       when a test fails.

   Code is a tree. What comes after a node is its `next`, so a function is
   run from its first node on without going back up: nothing in it is ever
   in the middle of something else but the value a C_EVAL waits for. Code
   to go to on a failed test is shared by the tests that go there. */

enum CodeKind {
    /* A value that takes no work to get: an argument is one of these. */
    C_LOCAL,     /* the value in `slot` */
    C_CONST,     /* `value`, which is not in the collector's memory */
    C_GLOBAL,    /* the top-level variable `global` */
    C_CLOSURE,   /* a new closure or thunk of `function`, capturing the
                    `count` values in `slots` */
    C_CONSTRUCT, /* a new constructor `tag` with the `count` fields `codes` */
    C_TYPE,      /* `type`, with what the `count` variables `bindings` stand
                    for taken from `slots` */

    /* What takes work. */
    C_METHOD,    /* the class method `global` at `type` (as C_TYPE) */
    C_STRING,    /* the list of the characters of `string` */
    C_INTEGER,   /* a literal beyond a small number: see Expr.big */
    C_CALL,      /* `first` (a C_LOCAL, C_CONST, C_GLOBAL or C_METHOD)
                    applied to the `count` arguments `codes` */
    C_ENTER,     /* runs the body of `function`, which takes no parameters,
                    with the values in `slots` captured: only in a C_EVAL */
    C_EVAL,      /* evaluates `first` (a value that takes no work, a C_METHOD,
                    C_CALL or C_ENTER), puts the result in `slot`, and goes
                    on with `next`. The `count` slots in `slots` are not
                    used after it, and are emptied so that they keep
                    nothing in use meanwhile. */
    C_LET,       /* puts the `count` values `codes` in the slots `slots`.
                    The closures and thunks among them may capture any of
                    those slots. */
    C_FIELDS,    /* puts the `count` fields of the constructor in `slot`
                    whose indices are in `slots` into the slots from `tag`
                    on */
    C_TEST,      /* `first` if the constructor in `slot` has tag `tag`, else
                    `next` */
    C_TEST_CHAR, /* the same for the character or small number `tag` */
    C_SWITCH,    /* codes[tag of the constructor in `slot`], or `next` if
                    that is NULL */
    C_FAIL       /* no alternative matches */
};

struct Code {
    unsigned char kind; /* an enum CodeKind */
    int slot;
    int count;
    int tag;
    Code *first;
    Code *next;
    Code **codes;
    int *slots;
    union {
        Value *value;         /* C_CONST */
        Global *global;       /* C_GLOBAL, C_METHOD */
        Function *function;   /* C_CLOSURE, C_ENTER */
        const char *string;   /* C_STRING; C_INTEGER with tag 2 */
        long number;          /* C_INTEGER with tag 1 */
    } u;
    Type *type;               /* C_TYPE, C_METHOD */
    Binding **bindings;       /* C_TYPE, C_METHOD */
    /* C_METHOD at a type without variables: the method found, while
       all_instances is `method_instances` (eval.c). */
    Value *method;
    const void *method_instances;
    const Token *token;       /* where in the program it comes from */
    int id;                   /* compile.c: its number in its function */
};

/* A function, or the code of a thunk (no parameters). Its locals are
   `slot_count` slots: first the parameters, then the captured variables,
   then what the body binds. */
struct Function {
    int arity;
    int capture_count;
    int slot_count;
    Code *body;
};

/* compile.c: the value of the top-level variable, whose definition has
   been elaborated: a closure if it is a function, else a thunk. */
Value *compile_global(Global *global);

/* eval.c: values for compile.c. The first three are outside the
   collector's memory. */
Value *constant_int(long number);
Value *constant_char(long code);
Value *constant_closure(const Function *function);
/* The constructor without its fields, or NULL if that is only made in the
   collector's memory. */
Value *constant_constructor(const Constructor *constructor);
Value *make_thunk(const Function *function);
/* A thunk that the collector keeps for good, for code to refer to. */
Value *shared_thunk(const Function *function);

/* A C function callable from Haskell through `foreign import prim`. It gets
   `arity` unevaluated arguments and may call force and apply. If its type
   has type variables, their types are its first arguments.

   IO: a value of type `IO a` is a function from a world token to an
   `IOResult a`, a box around the `a` (lib/Prelude.hs). A builtin that does
   I/O therefore takes the world token as an extra, last argument, has its
   effect when that argument arrives, and returns make_io_result. The box
   lets an action finish without evaluating its result. */
struct Builtin {
    const char *name;
    int arity;
    Value *(*function)(Value **arguments);
};

/* A constructor without fields is made once for each tag below this,
   outside the collector's memory. */
#define SHARED_TAGS 64

/* Constructor tags of the types the interpreter itself knows about. */
#define TAG_UNIT 0
#define TAG_NIL 0
#define TAG_CONS 1
#define TAG_FALSE 0
#define TAG_TRUE 1
#define TAG_NOTHING 0
#define TAG_JUST 1

Value *make_int(long number); /* may be shared: never change it */
Value *make_unshared_int(long number); /* for one that is changed */
Value *make_char(long code);
Value *make_con(int tag, int field_count);
Value *make_builtin(const Builtin *builtin);
Value *make_constructor(const Constructor *constructor);
Value *make_io_result(Value *result);
Value *make_type(Type *type);
Value *make_string(const char *string); /* a list of characters */
Value *make_bool(int condition);
Value *make_maybe(Value *value); /* Nothing for NULL */

/* A Haskell String as a C string, in UTF-8. Sets `*length` if not NULL,
   since the string may contain NUL characters. */
char *c_string(Value *list, long *length);

/* Evaluates to weak head normal form. The result is never a thunk. */
Value *force(Value *value);
/* Applies a function to one argument and evaluates the result. */
Value *apply(Value *function, Value *argument);

/* Exceptions. catch_exception applies the function to the argument, as
   apply does. If that raises an exception, it returns NULL and sets
   `*exception`. raise_exception does not return. */
Value *catch_exception(Value *function, Value *argument, Value **exception);
void raise_exception(Value *exception);

/* gc.c: memory that is freed when nothing refers to it any more. Everything
   the running program makes is allocated here, zeroed. gc_alloc_bytes is
   for memory that holds no pointers: bytes, digits, C strings.
   gc_alloc_value is for a Value: the collector then looks only at the
   fields that can point to its memory. It makes room for `field_count`
   pointers after the value: the fields of a V_CON (make_con), the captured
   variables of a closure or thunk, the arguments of a partial
   application.

   A collection can happen in any of the allocating functions, so a
   pointer to collected memory must be where the collector looks when one
   of them is called: in a C local, in collected memory that is itself
   reachable, or in what mark_roots names. Memory from xmalloc is not
   looked into. */
void *gc_alloc(size_t size);
void *gc_alloc_bytes(size_t size);
Value *gc_alloc_value(int field_count);
void *gc_realloc(void *old, size_t size, int bytes);
void gc_start(const void *bottom);
void gc_collect(void);
void gc_mark(const void *pointer);
/* Says that a pointer was put into memory from here that was made before:
   see "Generations" in gc.c. */
void gc_written(const void *address);
/* gc_written for a value: only an old one needs it, which the value
   itself says, so most values need no look into the collector's tables. */
#define gc_written_value(value) \
    ((value)->old ? gc_written(value) : (void)0)
void gc_mark_range(const void *start, const void *end);
/* eval.c: marks what the machine itself holds on to. */
void mark_roots(void);
/* eval.c: prints where in the program the machine is. */
void print_position(void);

/* integer.c: Integer beyond a C long. A value with this tag is one; see
   integer.c for what is in it. The arguments of these functions are
   Integers, big or not, and so are their results. */
#define INTEGER_BIG 1
int integer_is_big(Value *value);
Value *integer_from_unsigned(unsigned long number);
Value *integer_from_literal(const char *text);
Value *integer_arithmetic(char operation, Value *left, Value *right);
Value *integer_division(char operation, Value *left, Value *right);
int integer_compare(Value *left, Value *right);
Value *integer_shift(Value *value, long count, int left);
Value *integer_bitwise(char operation, Value *left, Value *right);
unsigned long integer_low_bits(Value *value);
char *integer_show(Value *value);
void integer_top_bits(Value *value, unsigned long *top, long *shift,
                      int *negative);

/* numbers.c: the arithmetic builtins. */
Value *builtin_numAdd(Value **arguments);
Value *builtin_numSub(Value **arguments);
Value *builtin_numMul(Value **arguments);
Value *builtin_numQuot(Value **arguments);
Value *builtin_numRem(Value **arguments);
Value *builtin_numDiv(Value **arguments);
Value *builtin_numMod(Value **arguments);
Value *builtin_numEq(Value **arguments);
Value *builtin_numLess(Value **arguments);
Value *builtin_numAnd(Value **arguments);
Value *builtin_numOr(Value **arguments);
Value *builtin_numXor(Value **arguments);
Value *builtin_numComplement(Value **arguments);
Value *builtin_numShiftL(Value **arguments);
Value *builtin_numShiftR(Value **arguments);
Value *builtin_numConvert(Value **arguments);
Value *builtin_numShow(Value **arguments);
Value *builtin_floatDivide(Value **arguments);
Value *builtin_floatMantissa(Value **arguments);
Value *builtin_floatExponent(Value **arguments);
Value *builtin_floatEncode(Value **arguments);
Value *builtin_floatClass(Value **arguments);
Value *builtin_floatMath(Value **arguments);
Value *builtin_floatPower(Value **arguments);
Value *builtin_floatAtan2(Value **arguments);
Value *builtin_floatConvert(Value **arguments);

/* derive.c: the method of an instance that comes from a `deriving` clause,
   at the given type. NULL if the class's default definition is to be used. */
Value *derived_method(const Instance *instance, Global *method, Type *type);
Value *builtin_deepForce(Value **arguments);
Value *builtin_dataFields(Value **arguments);
Value *builtin_dataRebuild(Value **arguments);

/* bytes.c: ByteString, and reading and writing files. */
Value *builtin_bytesPack(Value **arguments);
Value *builtin_bytesUnpack(Value **arguments);
Value *builtin_bytesLength(Value **arguments);
Value *builtin_bytesIndex(Value **arguments);
Value *builtin_bytesSlice(Value **arguments);
Value *builtin_bytesConcat(Value **arguments);
Value *builtin_bytesCompare(Value **arguments);
Value *builtin_bytesFind(Value **arguments);
Value *builtin_textCharAt(Value **arguments);
Value *builtin_textCount(Value **arguments);
Value *builtin_textOffset(Value **arguments);
Value *builtin_bytesFromString(Value **arguments);
Value *builtin_bytesNew(Value **arguments);
Value *builtin_sha256(Value **arguments);
Value *builtin_md5(Value **arguments);

/* files.c: file handles. handle_stream gives the C stream of a handle. */
FILE *handle_stream(long handle);
Value *builtin_handleOpen(Value **arguments);
Value *builtin_handleClose(Value **arguments);
Value *builtin_handleWrite(Value **arguments);
Value *builtin_handleFlush(Value **arguments);
Value *builtin_handleIsEOF(Value **arguments);
Value *builtin_handleRead(Value **arguments);
Value *builtin_processId(Value **arguments);
Value *builtin_environment(Value **arguments);
Value *builtin_fileModified(Value **arguments);
/* posix.c: runs the function on a stack of its own, large enough for deep
   recursion, and returns what it returns. */
int run_with_big_stack(int (*function)(int, char **), int argc, char **argv);
Value *builtin_charInfo(Value **arguments);
Value *builtin_bytesPoke(Value **arguments);
Value *builtin_bytesBlit(Value **arguments);
Value *builtin_bytesValidUtf8(Value **arguments);
Value *builtin_bytesToString(Value **arguments);
Value *builtin_bytesReadFile(Value **arguments);
Value *builtin_bytesWriteFile(Value **arguments);
Value *builtin_bytesHPut(Value **arguments);
Value *builtin_stringReadFile(Value **arguments);
Value *builtin_stringWriteFile(Value **arguments);
Value *builtin_interpreterTodo(Value **arguments);

/* posix.c: directories, file status and running programs. */
Value *builtin_pathKind(Value **arguments);
Value *builtin_fileSize(Value **arguments);
Value *builtin_isExecutable(Value **arguments);
Value *builtin_listDirectory(Value **arguments);
Value *builtin_makeDirectory(Value **arguments);
Value *builtin_removePath(Value **arguments);
Value *builtin_renamePath(Value **arguments);
Value *builtin_currentDirectory(Value **arguments);
Value *builtin_realPath(Value **arguments);
Value *builtin_temporaryFile(Value **arguments);
Value *builtin_runProcess(Value **arguments);
Value *builtin_sleepMicroseconds(Value **arguments);
Value *builtin_monotonicNanoseconds(Value **arguments);

/* survey.c */
void run_survey(const char *file);

/* builtins.c */
/* The command line of the Haskell program, for getArgs. */
void set_program_arguments(int count, char **arguments);
void set_program_path(const char *path);
const Builtin *find_builtin(const char *name);

/* instances.c: the definition of the class method for the type the class is
   used at. */
Value *find_method(Global *method, Type *type);
/* Whether find_method remembers what it finds for the type. */
int method_is_remembered(Type *type);
void mark_remembered_methods(void); /* for the collector */

/* elaborate.c: the value of a top-level variable, preparing it on first
   use. */
Value *global_value(Global *global);

#endif
