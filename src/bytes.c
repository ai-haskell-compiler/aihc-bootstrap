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
            text = gc_realloc(text, (size_t)capacity, 1);
        }
        used += utf8_encode_exact(force(list->fields[0])->number, text + used);
        list = force(list->fields[1]);
    }
    text = gc_realloc(text, (size_t)used + 1, 1);
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
    bytes = gc_alloc_bytes((size_t)*length + 1);
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
            bytes = gc_realloc(bytes, (size_t)capacity, 1);
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

/* ---- Text: bytes that are UTF-8 ----

   A Text is a ByteString that is known to be UTF-8 (lib/Data/Text.hs).
   An offset into it is in bytes, and is always where a character
   starts. */

/* textCharAt :: ByteString -> Int -> Char
   The character that starts at the offset. */
Value *builtin_textCharAt(Value **arguments)
{
    Value *bytes = bytes_argument(arguments[0]);
    long offset = force(arguments[1])->number;
    const char *p;

    if (offset < 0 || offset >= bytes->number) {
        die("Text offset %ld is out of range (%ld bytes)", offset,
            bytes->number);
    }
    p = (const char *)bytes->bytes + offset;
    return make_char(utf8_decode(&p));
}

/* textWidthAt :: ByteString -> Int -> Int
   How many bytes the character that starts at the offset takes. */
Value *builtin_textWidthAt(Value **arguments)
{
    Value *bytes = bytes_argument(arguments[0]);
    long offset = force(arguments[1])->number;
    const char *start;
    const char *p;

    if (offset < 0 || offset >= bytes->number) {
        die("Text offset %ld is out of range (%ld bytes)", offset,
            bytes->number);
    }
    start = (const char *)bytes->bytes + offset;
    p = start;
    utf8_decode(&p);
    return make_int(p - start);
}

/* textScan :: (Char -> Bool) -> ByteString -> Int -> Int -> Int
   The offset of the first character from `offset` on that the test does
   not hold for, or `end`. */
Value *builtin_textScan(Value **arguments)
{
    Value *test = arguments[0];
    Value *bytes = bytes_argument(arguments[1]);
    long end = force(arguments[2])->number;
    long offset = force(arguments[3])->number;
    const char *base = (const char *)bytes->bytes;

    if (end > bytes->number) {
        end = bytes->number;
    }
    while (offset >= 0 && offset < end) {
        const char *p = base + offset;
        long c = utf8_decode(&p);

        if (apply(test, make_char(c))->tag != TAG_TRUE) {
            break;
        }
        offset = p - base;
    }
    return make_int(offset);
}

/* Whether a character starts with the byte: the bytes after the first one
   of a character are 10xxxxxx. */
static int starts_character(unsigned char byte)
{
    return (byte & 0xC0) != 0x80;
}

/* textCount :: ByteString -> Int
   How many characters there are. */
Value *builtin_textCount(Value **arguments)
{
    Value *bytes = bytes_argument(arguments[0]);
    long count = 0;
    long i;

    for (i = 0; i < bytes->number; i++) {
        count += starts_character(bytes->bytes[i]);
    }
    return make_int(count);
}

/* textOffset :: ByteString -> Int -> Int
   The offset after that many characters: where the next one starts, or
   the length if there are no more than that. */
Value *builtin_textOffset(Value **arguments)
{
    Value *bytes = bytes_argument(arguments[0]);
    long count = force(arguments[1])->number;
    long i = 0;

    while (i < bytes->number && count > 0) {
        i++;
        while (i < bytes->number && !starts_character(bytes->bytes[i])) {
            i++;
        }
        count--;
    }
    return make_int(i);
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
            result = gc_realloc(result, (size_t)capacity, 1);
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

/* Bytes that are changed in place: for building a file in pieces. Once
   nothing changes them any more they are a ByteString like any other.

   bytesNew :: Int -> IO ByteString        (all zero) */
Value *builtin_bytesNew(Value **arguments)
{
    long length = force(arguments[0])->number;
    unsigned char *bytes = gc_alloc_bytes((size_t)(length < 0 ? 0 : length) + 1);

    if (length > 0) {
        memset(bytes, 0, (size_t)length);
    }
    return make_io_result(make_bytes(bytes, length < 0 ? 0 : length));
}

/* bytesPoke :: ByteString -> Int -> Word8 -> IO () */
Value *builtin_bytesPoke(Value **arguments)
{
    Value *bytes = bytes_argument(arguments[0]);
    long index = force(arguments[1])->number;

    if (index < 0 || index >= bytes->number) {
        die("byte index %ld is out of range (length %ld)", index,
            bytes->number);
    }
    ((unsigned char *)bytes->bytes)[index] =
        (unsigned char)force(arguments[2])->number;
    return make_io_result(make_con(TAG_UNIT, 0));
}

/* bytesBlit :: ByteString -> Int -> ByteString -> Int -> Int -> IO ()
   Copies that many bytes into the first byte string, starting at its
   index, from the second one, starting at its index. They may be the same
   one. */
Value *builtin_bytesBlit(Value **arguments)
{
    Value *target = bytes_argument(arguments[0]);
    long to = force(arguments[1])->number;
    Value *source = bytes_argument(arguments[2]);
    long from = force(arguments[3])->number;
    long count = force(arguments[4])->number;

    if (count > 0) {
        if (to < 0 || from < 0 || to + count > target->number
            || from + count > source->number) {
            die("copying %ld bytes goes out of range", count);
        }
        memmove((unsigned char *)target->bytes + to, source->bytes + from,
                (size_t)count);
    }
    return make_io_result(make_con(TAG_UNIT, 0));
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
    Value *bytes = bytes_argument(arguments[1]);

    fwrite(bytes->bytes, 1, (size_t)bytes->number,
           handle_stream(force(arguments[0])->number));
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
