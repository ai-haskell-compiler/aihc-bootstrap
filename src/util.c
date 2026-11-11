#include "util.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void (*failure_handler)(const char *message, const char *location) =
    NULL;

void set_failure_handler(void (*handler)(const char *message,
                                         const char *location))
{
    failure_handler = handler;
}

int has_failure_handler(void)
{
    return failure_handler != NULL;
}

void die(const char *format, ...)
{
    static char message[8192]; /* our messages are short */
    va_list arguments;

    va_start(arguments, format);
    vsprintf(message, format, arguments);
    va_end(arguments);
    if (failure_handler != NULL) {
        failure_handler(message, NULL);
    }
    fflush(stdout);
    fprintf(stderr, "%s\n", message);
    exit(1);
}

void todo(const char *what, const char *hs_file, int hs_line, int hs_col,
          const char *c_file, int c_line)
{
    static char message[8192];
    static char location[4096];

    sprintf(message, "TODO: %s", what);
    if (hs_file != NULL) {
        sprintf(location, "%s:%d:%d", hs_file, hs_line, hs_col);
    }
    if (failure_handler != NULL) {
        failure_handler(message, hs_file != NULL ? location : NULL);
    }
    fflush(stdout);
    fprintf(stderr, "%s", message);
    if (hs_file != NULL) {
        fprintf(stderr, " at %s", location);
    }
    fprintf(stderr, " (%s:%d)\n", c_file, c_line);
    exit(1);
}

void *xmalloc(size_t size)
{
    void *pointer = malloc(size);

    if (pointer == NULL) {
        die("out of memory");
    }
    return pointer;
}

void *xrealloc(void *pointer, size_t size)
{
    pointer = realloc(pointer, size);
    if (pointer == NULL) {
        die("out of memory");
    }
    return pointer;
}

char *copy_string(const char *start, size_t length)
{
    char *copy = xmalloc(length + 1);

    memcpy(copy, start, length);
    copy[length] = '\0';
    return copy;
}

char *read_file(const char *path)
{
    FILE *file = fopen(path, "rb");
    long size;
    char *contents;

    if (file == NULL) {
        return NULL;
    }
    fseek(file, 0, SEEK_END);
    size = ftell(file);
    fseek(file, 0, SEEK_SET);
    contents = xmalloc((size_t)size + 1);
    if (fread(contents, 1, (size_t)size, file) != (size_t)size) {
        die("cannot read %s", path);
    }
    contents[size] = '\0';
    fclose(file);
    return contents;
}

int utf8_encode(long code, char *out)
{
    if (code < 0x80 && code != 0) {
        out[0] = (char)code;
        return 1;
    }
    if (code < 0x800) {
        /* This includes the NUL character, written as two bytes so that it
           can be inside a C string. utf8_decode reads it back. */
        out[0] = (char)(0xC0 | (code >> 6));
        out[1] = (char)(0x80 | (code & 0x3F));
        return 2;
    }
    if (code < 0x10000) {
        out[0] = (char)(0xE0 | (code >> 12));
        out[1] = (char)(0x80 | ((code >> 6) & 0x3F));
        out[2] = (char)(0x80 | (code & 0x3F));
        return 3;
    }
    out[0] = (char)(0xF0 | (code >> 18));
    out[1] = (char)(0x80 | ((code >> 12) & 0x3F));
    out[2] = (char)(0x80 | ((code >> 6) & 0x3F));
    out[3] = (char)(0x80 | (code & 0x3F));
    return 4;
}

int utf8_encode_exact(long code, char *out)
{
    if (code == 0) {
        out[0] = '\0';
        return 1;
    }
    return utf8_encode(code, out);
}

long utf8_decode(const char **text)
{
    const unsigned char *p = (const unsigned char *)*text;
    long code = p[0];
    int extra = 0;
    int i;

    if (code >= 0xF0) {
        code &= 0x07;
        extra = 3;
    } else if (code >= 0xE0) {
        code &= 0x0F;
        extra = 2;
    } else if (code >= 0xC0) {
        code &= 0x1F;
        extra = 1;
    }
    for (i = 1; i <= extra; i++) {
        code = (code << 6) | (p[i] & 0x3F);
    }
    *text += extra + 1;
    return code;
}
