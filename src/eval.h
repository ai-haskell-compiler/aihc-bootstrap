#ifndef EVAL_H
#define EVAL_H

#include "syntax.h"

typedef struct Value Value;
typedef struct Env Env;
typedef struct Builtin Builtin;

enum ValueKind {
    V_INT,
    V_CHAR,
    V_CON,         /* a data constructor applied to all its fields */
    V_CLOSURE,     /* a lambda together with the variables it uses */
    V_BUILTIN,     /* a C function applied to fewer arguments than it needs */
    V_PARTIAL_CON, /* a constructor applied to fewer fields than it has */
    V_TYPE,        /* a type, passed to a polymorphic function */
    V_BYTES,       /* a ByteString: `number` bytes at `bytes` (bytes.c) */
    V_THUNK,       /* an expression that has not been evaluated yet */
    V_BLACKHOLE,   /* a thunk that is being evaluated right now */
    V_INDIRECTION  /* a thunk that has been evaluated; see `result` */
};

/* Every kind uses only some of the fields, as with Expr. Everything is boxed:
   there are no unboxed values anywhere in the interpreter. */
struct Value {
    enum ValueKind kind;
    long number;            /* V_INT, V_CHAR */
    int tag;                /* V_CON: index of the constructor in its type */
    int field_count;        /* V_CON */
    Value **fields;         /* V_CON */
    Binding *parameter;     /* V_CLOSURE */
    Expr *body;             /* V_CLOSURE */
    Expr *expr;             /* V_THUNK */
    Env *env;               /* V_CLOSURE, V_THUNK */
    const Builtin *builtin; /* V_BUILTIN */
    const Constructor *constructor; /* V_PARTIAL_CON */
    int argument_count;     /* V_BUILTIN, V_PARTIAL_CON */
    Value **arguments;      /* V_BUILTIN, V_PARTIAL_CON */
    Value *result;          /* V_INDIRECTION */
    Type *type;             /* V_TYPE: has no TY_RIGID in it */
    const unsigned char *bytes; /* V_BYTES */
};

/* The values of the local variables in scope, innermost first. */
struct Env {
    Binding *binding;
    Value *value;
    Env *outer;
};

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

/* Constructor tags of the types the interpreter itself knows about. */
#define TAG_UNIT 0
#define TAG_NIL 0
#define TAG_CONS 1
#define TAG_FALSE 0
#define TAG_TRUE 1
#define TAG_NOTHING 0
#define TAG_JUST 1

Value *make_int(long number);
Value *make_char(long code);
Value *make_con(int tag, int field_count);
Value *make_thunk(Expr *expr, Env *env);
Value *make_builtin(const Builtin *builtin);
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

/* derive.c: the method of an instance that comes from a `deriving` clause,
   at the given type. NULL if the class's default definition is to be used. */
Value *derived_method(const Instance *instance, Global *method, Type *type);
Value *builtin_deepForce(Value **arguments);

/* bytes.c: ByteString, and reading and writing files. */
Value *builtin_bytesPack(Value **arguments);
Value *builtin_bytesUnpack(Value **arguments);
Value *builtin_bytesLength(Value **arguments);
Value *builtin_bytesIndex(Value **arguments);
Value *builtin_bytesSlice(Value **arguments);
Value *builtin_bytesConcat(Value **arguments);
Value *builtin_bytesCompare(Value **arguments);
Value *builtin_bytesFind(Value **arguments);
Value *builtin_bytesFromString(Value **arguments);
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
const Builtin *find_builtin(const char *name);

/* instances.c: the definition of the class method for the type the class is
   used at. */
Value *find_method(Global *method, Type *type);

/* elaborate.c: the value of a top-level variable, preparing it on first
   use. */
Value *global_value(Global *global);

#endif
