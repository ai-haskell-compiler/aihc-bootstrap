#ifndef UTIL_H
#define UTIL_H

#include <stddef.h>

/* Prints the message and exits. Used for things that cannot happen in valid
   input; we do not try to produce good error messages. */
void die(const char *format, ...);

/* Reports a Haskell feature the interpreter does not support yet and exits.
   This is the signal that drives development: run AIHC, see what is missing,
   implement it, repeat. `hs_file` may be NULL when there is no source
   position. */
void todo(const char *what, const char *hs_file, int hs_line, int hs_col,
          const char *c_file, int c_line);

#define TODO(what) todo((what), NULL, 0, 0, __FILE__, __LINE__)

/* Normally die and todo print their message and exit. With a failure
   handler they call it instead, and it must not return. Survey mode uses
   this to carry on with the next definition. `location` is where in the
   Haskell source, or NULL. */
void set_failure_handler(void (*handler)(const char *message,
                                         const char *location));
int has_failure_handler(void);

/* Memory is never freed: there is no garbage collector yet. */
void *xmalloc(size_t size);
void *xrealloc(void *pointer, size_t size);
char *copy_string(const char *start, size_t length);

/* Characters are Unicode code points. Strings in C are UTF-8.
   utf8_encode writes the code point to `out` (up to 4 bytes) and returns
   how many bytes that took. utf8_decode reads one code point at `*text` and
   moves `*text` past it. */
int utf8_encode(long code, char *out);
/* The same, except that the NUL character is written as one zero byte, as
   a file wants it, where utf8_encode writes two bytes to keep it out of C
   strings. */
int utf8_encode_exact(long code, char *out);
long utf8_decode(const char **text);

/* Returns the contents of the file, or NULL if it cannot be opened. */
char *read_file(const char *path);

#endif
