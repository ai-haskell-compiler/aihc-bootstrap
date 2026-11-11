#include "eval.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* The C functions Haskell code can reach with `foreign import prim "name"`.
   This table is also where C code from packages will go: it is linked into
   the interpreter, so nothing is loaded at run time.

   The comment on each function is its Haskell type. Type variables come
   first in `arguments`, and an IO action gets the world token last. */

static int program_argument_count = 0;
static char **program_arguments = NULL;

static const char *program_path = "aihc-bootstrap";

void set_program_path(const char *path)
{
    program_path = path;
}

/* programPath :: IO String
   How the interpreter was started: its argv[0]. */
static Value *builtin_programPath(Value **arguments)
{
    (void)arguments;
    return make_io_result(make_string(program_path));
}

void set_program_arguments(int count, char **arguments)
{
    program_argument_count = count;
    program_arguments = arguments;
}

/* getArgs :: IO [String] */
static Value *builtin_getArgs(Value **arguments)
{
    Value *list = make_con(TAG_NIL, 0);
    int i;

    (void)arguments;
    for (i = program_argument_count - 1; i >= 0; i--) {
        Value *cell = make_con(TAG_CONS, 2);

        cell->fields[0] = make_string(program_arguments[i]);
        cell->fields[1] = list;
        list = cell;
    }
    return make_io_result(list);
}

/* lookupEnv :: String -> IO (Maybe String) */
static Value *builtin_lookupEnv(Value **arguments)
{
    const char *value = getenv(c_string(arguments[0], NULL));
    Value *result;

    if (value == NULL) {
        return make_io_result(make_con(TAG_NOTHING, 0));
    }
    result = make_con(TAG_JUST, 1);
    result->fields[0] = make_string(value);
    return make_io_result(result);
}

/* os :: String        The names are the ones GHC uses. */
static Value *builtin_systemOs(Value **arguments)
{
    (void)arguments;
#if defined(__APPLE__)
    return make_string("darwin");
#elif defined(__linux__)
    return make_string("linux");
#else
    TODO("System.Info.os on this system");
    return NULL;
#endif
}

/* arch :: String */
static Value *builtin_systemArch(Value **arguments)
{
    (void)arguments;
#if defined(__aarch64__) || defined(__arm64__)
    return make_string("aarch64");
#elif defined(__x86_64__)
    return make_string("x86_64");
#else
    TODO("System.Info.arch on this machine");
    return NULL;
#endif
}

/* A mutable cell (IORef, STRef, MVar) is a constructed value with one
   field, and writing replaces the field. */

/* newRef :: a -> IO (Ref a) */
static Value *builtin_newRef(Value **arguments)
{
    Value *cell = make_con(0, 1);

    cell->fields[0] = arguments[1];
    return make_io_result(cell);
}

/* readRef :: Ref a -> IO a */
static Value *builtin_readRef(Value **arguments)
{
    return make_io_result(force(arguments[1])->fields[0]);
}

/* writeRef :: Ref a -> a -> IO () */
static Value *builtin_writeRef(Value **arguments)
{
    force(arguments[1])->fields[0] = arguments[2];
    return make_io_result(make_con(TAG_UNIT, 0));
}

/* ---- Arrays ----

   An array is a value with one field for each element, which the builtins
   change in place. An immutable array is the same thing that nobody
   changes any more. Every builtin takes the type of the elements first,
   as every polymorphic builtin does. */

static Value *array_argument(Value *value, long index)
{
    value = force(value);
    if (index < 0 || index >= value->field_count) {
        die("array index %ld is out of range (length %d)", index,
            value->field_count);
    }
    return value;
}

/* arrayNew :: Int -> a -> IO (MutableArray a)
   An array of that length with every element the given value. */
static Value *builtin_arrayNew(Value **arguments)
{
    long length = force(arguments[1])->number;
    Value *array = make_con(0, length < 0 ? 0 : (int)length);
    long i;

    for (i = 0; i < length; i++) {
        array->fields[i] = arguments[2];
    }
    return make_io_result(array);
}

/* arrayRead :: MutableArray a -> Int -> IO a */
static Value *builtin_arrayRead(Value **arguments)
{
    long index = force(arguments[2])->number;

    return make_io_result(array_argument(arguments[1], index)->fields[index]);
}

/* arrayWrite :: MutableArray a -> Int -> a -> IO () */
static Value *builtin_arrayWrite(Value **arguments)
{
    long index = force(arguments[2])->number;

    array_argument(arguments[1], index)->fields[index] = arguments[3];
    return make_io_result(make_con(TAG_UNIT, 0));
}

/* arrayLength :: MutableArray a -> Int */
static Value *builtin_arrayLength(Value **arguments)
{
    return make_int(force(arguments[1])->field_count);
}

/* arrayIndex :: MutableArray a -> Int -> a
   For an array that is not changed any more. */
static Value *builtin_arrayIndex(Value **arguments)
{
    long index = force(arguments[2])->number;

    return force(array_argument(arguments[1], index)->fields[index]);
}

/* arrayCopy :: MutableArray a -> Int -> MutableArray a -> Int -> Int -> IO ()
   Copies that many elements from the first array, starting at its index,
   into the second one, starting at its index. The arrays may be the same
   one. */
static Value *builtin_arrayCopy(Value **arguments)
{
    Value *source = force(arguments[1]);
    long from = force(arguments[2])->number;
    Value *target = force(arguments[3]);
    long to = force(arguments[4])->number;
    long count = force(arguments[5])->number;

    if (count > 0) {
        array_argument(source, from);
        array_argument(source, from + count - 1);
        array_argument(target, to);
        array_argument(target, to + count - 1);
        memmove(target->fields + to, source->fields + from,
                (size_t)count * sizeof(Value *));
    }
    return make_io_result(make_con(TAG_UNIT, 0));
}

/* realWorld :: World
   The token an IO action is applied to. Any value would do. */
static Value *builtin_realWorld(Value **arguments)
{
    (void)arguments;
    return make_con(TAG_UNIT, 0);
}

/* seq :: a -> b -> b */
static Value *builtin_seq(Value **arguments)
{
    force(arguments[2]);
    return arguments[3];
}

/* unsafeCoerce :: a -> b */
static Value *builtin_unsafeCoerce(Value **arguments)
{
    return force(arguments[2]);
}

/* hPutStr :: Int -> String -> IO ()
   The Int is a file descriptor: 1 for stdout, 2 for stderr. */
static Value *builtin_hPutStr(Value **arguments)
{
    FILE *file = handle_stream(force(arguments[0])->number);
    Value *list = force(arguments[1]);

    while (list->tag == TAG_CONS) {
        char bytes[4];
        int count = utf8_encode_exact(force(list->fields[0])->number, bytes);

        fwrite(bytes, 1, (size_t)count, file);
        list = force(list->fields[1]);
    }
    return make_io_result(make_con(TAG_UNIT, 0));
}

/* exit :: Int -> IO a */
static Value *builtin_exit(Value **arguments)
{
    fflush(stdout);
    exit((int)force(arguments[1])->number);
    return NULL;
}

/* raise :: SomeException -> a */
static Value *builtin_raise(Value **arguments)
{
    raise_exception(arguments[1]);
    return NULL;
}

/* catchAny :: IO a -> (SomeException -> IO a) -> IO a */
static Value *builtin_catchAny(Value **arguments)
{
    Value *world = arguments[3];
    Value *exception;
    Value *result = catch_exception(arguments[1], world, &exception);

    if (result != NULL) {
        return result;
    }
    return apply(apply(arguments[2], exception), world);
}

/* cast :: a -> Maybe b
   Just the argument if a and b are the same type. This is what Typeable is
   for; here the types are simply at hand. */
static Value *builtin_cast(Value **arguments)
{
    Value *result;

    if (!types_equal(force(arguments[0])->type, force(arguments[1])->type)) {
        return make_con(TAG_NOTHING, 0);
    }
    result = make_con(TAG_JUST, 1);
    result->fields[0] = arguments[2];
    return result;
}

static const Builtin builtins[] = {
    { "getArgs", 1, builtin_getArgs },
    { "lookupEnv", 2, builtin_lookupEnv },
    { "systemOs", 0, builtin_systemOs },
    { "systemArch", 0, builtin_systemArch },
    { "seq", 4, builtin_seq },
    { "newRef", 3, builtin_newRef },
    { "readRef", 3, builtin_readRef },
    { "writeRef", 4, builtin_writeRef },
    { "arrayNew", 4, builtin_arrayNew },
    { "arrayRead", 4, builtin_arrayRead },
    { "arrayWrite", 5, builtin_arrayWrite },
    { "arrayLength", 2, builtin_arrayLength },
    { "arrayIndex", 3, builtin_arrayIndex },
    { "arrayCopy", 7, builtin_arrayCopy },
    { "realWorld", 0, builtin_realWorld },
    { "interpreterTodo", 2, builtin_interpreterTodo },
    { "bytesPack", 1, builtin_bytesPack },
    { "bytesUnpack", 1, builtin_bytesUnpack },
    { "bytesLength", 1, builtin_bytesLength },
    { "bytesIndex", 2, builtin_bytesIndex },
    { "bytesSlice", 3, builtin_bytesSlice },
    { "bytesConcat", 1, builtin_bytesConcat },
    { "bytesCompare", 2, builtin_bytesCompare },
    { "bytesFind", 2, builtin_bytesFind },
    { "bytesFromString", 1, builtin_bytesFromString },
    { "bytesNew", 2, builtin_bytesNew },
    { "sha256", 1, builtin_sha256 },
    { "handleOpen", 3, builtin_handleOpen },
    { "handleClose", 2, builtin_handleClose },
    { "handleWrite", 3, builtin_handleWrite },
    { "handleFlush", 2, builtin_handleFlush },
    { "handleIsEOF", 2, builtin_handleIsEOF },
    { "handleRead", 3, builtin_handleRead },
    { "processId", 1, builtin_processId },
    { "environment", 1, builtin_environment },
    { "fileModified", 2, builtin_fileModified },
    { "programPath", 1, builtin_programPath },
    { "charInfo", 2, builtin_charInfo },
    { "bytesPoke", 4, builtin_bytesPoke },
    { "bytesBlit", 6, builtin_bytesBlit },
    { "bytesValidUtf8", 1, builtin_bytesValidUtf8 },
    { "bytesToString", 1, builtin_bytesToString },
    { "bytesReadFile", 2, builtin_bytesReadFile },
    { "bytesWriteFile", 3, builtin_bytesWriteFile },
    { "bytesHPut", 3, builtin_bytesHPut },
    { "stringReadFile", 2, builtin_stringReadFile },
    { "stringWriteFile", 4, builtin_stringWriteFile },
    { "pathKind", 2, builtin_pathKind },
    { "fileSize", 2, builtin_fileSize },
    { "isExecutable", 2, builtin_isExecutable },
    { "listDirectory", 2, builtin_listDirectory },
    { "makeDirectory", 2, builtin_makeDirectory },
    { "removePath", 2, builtin_removePath },
    { "renamePath", 3, builtin_renamePath },
    { "currentDirectory", 1, builtin_currentDirectory },
    { "realPath", 2, builtin_realPath },
    { "temporaryFile", 2, builtin_temporaryFile },
    { "runProcess", 7, builtin_runProcess },
    { "sleepMicroseconds", 2, builtin_sleepMicroseconds },
    { "monotonicNanoseconds", 1, builtin_monotonicNanoseconds },
    { "deepForce", 2, builtin_deepForce },
    { "dataFields", 2, builtin_dataFields },
    { "dataRebuild", 3, builtin_dataRebuild },
    { "numAdd", 3, builtin_numAdd },
    { "numSub", 3, builtin_numSub },
    { "numMul", 3, builtin_numMul },
    { "numQuot", 3, builtin_numQuot },
    { "numRem", 3, builtin_numRem },
    { "numDiv", 3, builtin_numDiv },
    { "numMod", 3, builtin_numMod },
    { "numEq", 3, builtin_numEq },
    { "numLess", 3, builtin_numLess },
    { "numAnd", 3, builtin_numAnd },
    { "numOr", 3, builtin_numOr },
    { "numXor", 3, builtin_numXor },
    { "numComplement", 2, builtin_numComplement },
    { "numShiftL", 3, builtin_numShiftL },
    { "numShiftR", 3, builtin_numShiftR },
    { "numConvert", 3, builtin_numConvert },
    { "numShow", 2, builtin_numShow },
    { "floatDivide", 3, builtin_floatDivide },
    { "floatMantissa", 2, builtin_floatMantissa },
    { "floatExponent", 2, builtin_floatExponent },
    { "floatEncode", 3, builtin_floatEncode },
    { "floatClass", 2, builtin_floatClass },
    { "floatMath", 3, builtin_floatMath },
    { "floatPower", 3, builtin_floatPower },
    { "floatAtan2", 3, builtin_floatAtan2 },
    { "floatConvert", 3, builtin_floatConvert },
    { "unsafeCoerce", 3, builtin_unsafeCoerce },
    { "hPutStr", 3, builtin_hPutStr },
    { "exit", 3, builtin_exit },
    { "raise", 2, builtin_raise },
    { "catchAny", 4, builtin_catchAny },
    { "cast", 3, builtin_cast },
    { NULL, 0, NULL }
};

const Builtin *find_builtin(const char *name)
{
    int i;

    for (i = 0; builtins[i].name != NULL; i++) {
        if (strcmp(builtins[i].name, name) == 0) {
            return &builtins[i];
        }
    }
    fprintf(stderr, "unknown builtin: %s\n", name);
    TODO("builtin");
    return NULL;
}
