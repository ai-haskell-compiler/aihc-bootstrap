#include "eval.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ByteString: a V_BYTES value is a pointer to bytes and a length (`number`).
   The bytes are never changed, so a slice shares them with the value it was
   cut from. These are the operations that need the representation; the
   rest of Data.ByteString is written in Haskell on top of them
   (lib/Data/ByteString.hs).

   Strings of text are also read and written here, as UTF-8. */

static Value *make_bytes(const unsigned char *bytes, long length)
{
    Value *value = make_int(length);

    value->kind = V_BYTES;
    value->bytes = bytes;
    return value;
}

static Value *bytes_argument(Value *value)
{
    value = force(value);
    if (value->kind != V_BYTES) {
        die("a ByteString builtin was given something else");
    }
    return value;
}

Value *make_bool(int condition)
{
    return make_con(condition ? TAG_TRUE : TAG_FALSE, 0);
}

Value *make_maybe(Value *value)
{
    Value *result;

    if (value == NULL) {
        return make_con(TAG_NOTHING, 0);
    }
    result = make_con(TAG_JUST, 1);
    result->fields[0] = value;
    return result;
}

/* A Haskell String as a C string, in UTF-8. Sets `*length` if not NULL,
   since the string may contain NUL characters. */
char *c_string(Value *list, long *length)
{
    char *text = NULL;
    long used = 0;
    long capacity = 0;

    list = force(list);
    while (list->tag == TAG_CONS) {
        if (used + 5 > capacity) {
            capacity = capacity * 2 + 64;
            text = xrealloc(text, (size_t)capacity);
        }
        used += utf8_encode_exact(force(list->fields[0])->number, text + used);
        list = force(list->fields[1]);
    }
    text = xrealloc(text, (size_t)used + 1);
    text[used] = '\0';
    if (length != NULL) {
        *length = used;
    }
    return text;
}

/* The bytes as a Haskell String, decoded as UTF-8. */
static Value *string_from_bytes(const unsigned char *bytes, long length)
{
    const char *p = (const char *)bytes;
    const char *end = p + length;
    Value *list = make_con(TAG_NIL, 0);
    Value **last = &list;

    while (p < end) {
        Value *cell = make_con(TAG_CONS, 2);

        cell->fields[0] = make_char(utf8_decode(&p));
        cell->fields[1] = *last;
        *last = cell;
        last = &cell->fields[1];
    }
    return list;
}

/* The whole file, or NULL if it cannot be read. */
static unsigned char *read_whole_file(const char *path, long *length)
{
    FILE *file = fopen(path, "rb");
    unsigned char *bytes;

    if (file == NULL) {
        return NULL;
    }
    fseek(file, 0, SEEK_END);
    *length = ftell(file);
    fseek(file, 0, SEEK_SET);
    bytes = xmalloc((size_t)*length + 1);
    if (fread(bytes, 1, (size_t)*length, file) != (size_t)*length) {
        fclose(file);
        return NULL;
    }
    fclose(file);
    return bytes;
}

static int write_whole_file(const char *path, const char *mode,
                            const unsigned char *bytes, long length)
{
    FILE *file = fopen(path, mode);
    int ok;

    if (file == NULL) {
        return 0;
    }
    ok = fwrite(bytes, 1, (size_t)length, file) == (size_t)length;
    return fclose(file) == 0 && ok;
}

/* bytesPack :: [Word8] -> ByteString */
Value *builtin_bytesPack(Value **arguments)
{
    unsigned char *bytes = NULL;
    long length = 0;
    long capacity = 0;
    Value *list = force(arguments[0]);

    while (list->tag == TAG_CONS) {
        if (length == capacity) {
            capacity = capacity * 2 + 64;
            bytes = xrealloc(bytes, (size_t)capacity);
        }
        bytes[length++] = (unsigned char)force(list->fields[0])->number;
        list = force(list->fields[1]);
    }
    return make_bytes(bytes, length);
}

/* bytesUnpack :: ByteString -> [Word8] */
Value *builtin_bytesUnpack(Value **arguments)
{
    Value *bytes = bytes_argument(arguments[0]);
    Value *list = make_con(TAG_NIL, 0);
    long i;

    for (i = bytes->number - 1; i >= 0; i--) {
        Value *cell = make_con(TAG_CONS, 2);

        cell->fields[0] = make_int(bytes->bytes[i]);
        cell->fields[1] = list;
        list = cell;
    }
    return list;
}

/* bytesLength :: ByteString -> Int */
Value *builtin_bytesLength(Value **arguments)
{
    return make_int(bytes_argument(arguments[0])->number);
}

/* bytesIndex :: ByteString -> Int -> Word8 */
Value *builtin_bytesIndex(Value **arguments)
{
    Value *bytes = bytes_argument(arguments[0]);
    long index = force(arguments[1])->number;

    if (index < 0 || index >= bytes->number) {
        die("ByteString index %ld is out of range (length %ld)", index,
            bytes->number);
    }
    return make_int(bytes->bytes[index]);
}

/* bytesSlice :: Int -> Int -> ByteString -> ByteString
   `length` bytes from `offset`, as far as they exist. */
Value *builtin_bytesSlice(Value **arguments)
{
    long offset = force(arguments[0])->number;
    long length = force(arguments[1])->number;
    Value *bytes = bytes_argument(arguments[2]);

    if (offset < 0) {
        offset = 0;
    }
    if (offset > bytes->number) {
        offset = bytes->number;
    }
    if (length < 0) {
        length = 0;
    }
    if (length > bytes->number - offset) {
        length = bytes->number - offset;
    }
    return make_bytes(bytes->bytes + offset, length);
}

/* bytesConcat :: [ByteString] -> ByteString */
Value *builtin_bytesConcat(Value **arguments)
{
    unsigned char *result = NULL;
    long length = 0;
    long capacity = 0;
    Value *list = force(arguments[0]);

    while (list->tag == TAG_CONS) {
        Value *bytes = bytes_argument(list->fields[0]);

        if (length + bytes->number > capacity) {
            capacity = (length + bytes->number) * 2 + 64;
            result = xrealloc(result, (size_t)capacity);
        }
        if (bytes->number > 0) {
            memcpy(result + length, bytes->bytes, (size_t)bytes->number);
        }
        length += bytes->number;
        list = force(list->fields[1]);
    }
    return make_bytes(result, length);
}

/* bytesCompare :: ByteString -> ByteString -> Int
   Negative, zero or positive, comparing byte by byte. */
Value *builtin_bytesCompare(Value **arguments)
{
    Value *a = bytes_argument(arguments[0]);
    Value *b = bytes_argument(arguments[1]);
    long shorter = a->number < b->number ? a->number : b->number;
    int order = shorter > 0 ? memcmp(a->bytes, b->bytes, (size_t)shorter) : 0;

    if (order == 0) {
        order = a->number < b->number ? -1 : a->number > b->number ? 1 : 0;
    }
    return make_int(order < 0 ? -1 : order > 0 ? 1 : 0);
}

/* bytesFind :: ByteString -> ByteString -> Int
   Where the first argument first occurs in the second, or -1. */
Value *builtin_bytesFind(Value **arguments)
{
    Value *needle = bytes_argument(arguments[0]);
    Value *haystack = bytes_argument(arguments[1]);
    long i;

    for (i = 0; i + needle->number <= haystack->number; i++) {
        if (needle->number == 0
            || memcmp(haystack->bytes + i, needle->bytes,
                      (size_t)needle->number) == 0) {
            return make_int(i);
        }
    }
    return make_int(-1);
}

/* bytesFromString :: String -> ByteString        (UTF-8) */
Value *builtin_bytesFromString(Value **arguments)
{
    long length;
    char *text = c_string(arguments[0], &length);

    return make_bytes((const unsigned char *)text, length);
}

/* Whether the bytes are well-formed UTF-8. */
static int is_valid_utf8(const unsigned char *p, long length)
{
    long i = 0;

    while (i < length) {
        unsigned long code;
        int extra;
        int k;

        if (p[i] < 0x80) {
            i++;
            continue;
        }
        if (p[i] >= 0xF0 && p[i] <= 0xF4) {
            extra = 3;
            code = p[i] & 0x07u;
        } else if (p[i] >= 0xE0) {
            extra = 2;
            code = p[i] & 0x0Fu;
        } else if (p[i] >= 0xC2) {
            extra = 1;
            code = p[i] & 0x1Fu;
        } else {
            return 0;
        }
        if (i + extra >= length) {
            return 0;
        }
        for (k = 1; k <= extra; k++) {
            if ((p[i + k] & 0xC0) != 0x80) {
                return 0;
            }
            code = (code << 6) | (p[i + k] & 0x3Fu);
        }
        /* Not longer than needed, not a surrogate, not beyond Unicode. */
        if ((extra == 2 && code < 0x800) || (extra == 3 && code < 0x10000)
            || (code >= 0xD800 && code <= 0xDFFF) || code > 0x10FFFF) {
            return 0;
        }
        i += extra + 1;
    }
    return 1;
}

/* bytesValidUtf8 :: ByteString -> Bool */
Value *builtin_bytesValidUtf8(Value **arguments)
{
    Value *bytes = bytes_argument(arguments[0]);

    return make_bool(is_valid_utf8(bytes->bytes, bytes->number));
}

/* bytesToString :: ByteString -> String        (UTF-8, which must be valid) */
Value *builtin_bytesToString(Value **arguments)
{
    Value *bytes = bytes_argument(arguments[0]);

    return string_from_bytes(bytes->bytes, bytes->number);
}

/* bytesReadFile :: String -> IO (Maybe ByteString) */
Value *builtin_bytesReadFile(Value **arguments)
{
    long length = 0;
    unsigned char *bytes = read_whole_file(c_string(arguments[0], NULL), &length);

    return make_io_result(make_maybe(bytes == NULL ? NULL
                                                   : make_bytes(bytes, length)));
}

/* bytesWriteFile :: String -> ByteString -> IO Bool        (False: failed) */
Value *builtin_bytesWriteFile(Value **arguments)
{
    Value *bytes = bytes_argument(arguments[1]);

    return make_io_result(make_bool(write_whole_file(
        c_string(arguments[0], NULL), "wb", bytes->bytes, bytes->number)));
}

/* bytesHPut :: Int -> ByteString -> IO ()
   The Int is a file descriptor: 1 for stdout, 2 for stderr. */
Value *builtin_bytesHPut(Value **arguments)
{
    long descriptor = force(arguments[0])->number;
    Value *bytes = bytes_argument(arguments[1]);

    if (descriptor != 1 && descriptor != 2) {
        TODO("output to a file handle");
    }
    fwrite(bytes->bytes, 1, (size_t)bytes->number,
           descriptor == 2 ? stderr : stdout);
    return make_io_result(make_con(TAG_UNIT, 0));
}

/* stringReadFile :: String -> IO (Maybe String)        (UTF-8) */
Value *builtin_stringReadFile(Value **arguments)
{
    long length = 0;
    unsigned char *bytes = read_whole_file(c_string(arguments[0], NULL), &length);

    if (bytes == NULL) {
        return make_io_result(make_maybe(NULL));
    }
    if (!is_valid_utf8(bytes, length)) {
        TODO("reading a text file that is not UTF-8");
    }
    return make_io_result(make_maybe(string_from_bytes(bytes, length)));
}

/* stringWriteFile :: Bool -> String -> String -> IO Bool
   The Bool says whether to append. False as the result: failed. */
Value *builtin_stringWriteFile(Value **arguments)
{
    int append = force(arguments[0])->tag == TAG_TRUE;
    long length;
    char *text = c_string(arguments[2], &length);

    return make_io_result(make_bool(write_whole_file(
        c_string(arguments[1], NULL), append ? "ab" : "wb",
        (const unsigned char *)text, length)));
}

/* interpreterTodo :: String -> a
   For a library function that is only partly written: stops like any
   other unsupported feature, instead of giving a wrong answer. */
Value *builtin_interpreterTodo(Value **arguments)
{
    TODO(c_string(arguments[1], NULL));
    return NULL;
}
