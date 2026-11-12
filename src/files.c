#include "eval.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* File handles. A Handle is a number: 0, 1 and 2 are standard input,
   output and error, and the others are files opened with handleOpen.
   Everything read or written is bytes; text is UTF-8, which the Haskell
   side sees to (lib/System/IO.hs). */

static FILE **handles = NULL;
static int handle_count = 0;

FILE *handle_stream(long handle)
{
    if (handles == NULL) {
        handles = xmalloc(3 * sizeof(FILE *));
        handles[0] = stdin;
        handles[1] = stdout;
        handles[2] = stderr;
        handle_count = 3;
    }
    if (handle < 0 || handle >= handle_count || handles[handle] == NULL) {
        die("handle %ld is closed, or was never opened", handle);
    }
    return handles[handle];
}

static FILE *handle_file(Value *value)
{
    return handle_stream(force(value)->number);
}

static Value *make_byte_string(unsigned char *bytes, long length)
{
    Value *value = make_int(length);

    value->kind = V_BYTES;
    value->bytes = bytes;
    return value;
}

/* handleOpen :: String -> Int -> IO Int
   Opens the file: 0 to read, 1 to write (it is emptied or created), 2 to
   add to its end, 3 to read and write. The result is the handle, or -1. */
Value *builtin_handleOpen(Value **arguments)
{
    static const char *modes[] = { "rb", "wb", "ab", "r+b" };
    const char *path = c_string(arguments[0], NULL);
    long mode = force(arguments[1])->number;
    FILE *file;

    handle_file(make_int(1)); /* makes sure the table is there */
    file = fopen(path, modes[mode]);
    if (file == NULL && mode == 3) {
        file = fopen(path, "w+b"); /* read and write: created if missing */
    }
    if (file == NULL) {
        return make_io_result(make_int(-1));
    }
    handles = xrealloc(handles, (size_t)(handle_count + 1) * sizeof(FILE *));
    handles[handle_count] = file;
    return make_io_result(make_int(handle_count++));
}

/* handleClose :: Int -> IO ()
   Closing a handle twice does nothing, as in GHC. */
Value *builtin_handleClose(Value **arguments)
{
    long handle = force(arguments[0])->number;

    if (handle >= 3 && handle < handle_count && handles[handle] != NULL) {
        fclose(handles[handle]);
        handles[handle] = NULL;
    }
    return make_io_result(make_con(TAG_UNIT, 0));
}

/* handleWrite :: Int -> ByteString -> IO () */
Value *builtin_handleWrite(Value **arguments)
{
    FILE *file = handle_file(arguments[0]);
    Value *bytes = force(arguments[1]);

    fwrite(bytes->bytes, 1, (size_t)bytes->number, file);
    return make_io_result(make_con(TAG_UNIT, 0));
}

/* handleFlush :: Int -> IO () */
Value *builtin_handleFlush(Value **arguments)
{
    fflush(handle_file(arguments[0]));
    return make_io_result(make_con(TAG_UNIT, 0));
}

/* handleIsEOF :: Int -> IO Bool */
Value *builtin_handleIsEOF(Value **arguments)
{
    FILE *file = handle_file(arguments[0]);
    int c = getc(file);

    if (c != EOF) {
        ungetc(c, file);
    }
    return make_io_result(make_bool(c == EOF));
}

/* handleRead :: Int -> Bool -> IO (Maybe ByteString)
   With True: the bytes up to the end of the line, without the newline, or
   Nothing if there is nothing left. With False: all the bytes that are
   left. */
Value *builtin_handleRead(Value **arguments)
{
    FILE *file = handle_file(arguments[0]);
    int line = force(arguments[1])->tag == TAG_TRUE;
    unsigned char *bytes = NULL;
    long length = 0;
    long capacity = 0;
    int c = EOF;

    while ((c = getc(file)) != EOF && !(line && c == '\n')) {
        if (length == capacity) {
            capacity = capacity * 2 + 64;
            bytes = gc_realloc(bytes, (size_t)capacity, 1);
        }
        bytes[length++] = (unsigned char)c;
    }
    if (line && c == EOF && length == 0) {
        return make_io_result(make_maybe(NULL));
    }
    return make_io_result(make_maybe(make_byte_string(bytes, length)));
}
