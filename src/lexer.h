#ifndef LEXER_H
#define LEXER_H

#include "util.h"

enum TokenKind {
    T_VARID,    /* x, foldr, x' */
    T_CONID,    /* Just, Data.Map, Map.Map (dots are kept) */
    T_VARSYM,   /* +, >>=, . */
    T_RESERVED, /* reserved words and reserved operators: where, =, ->, :: */
    T_SPECIAL,  /* one of ( ) [ ] , ; ` { } */
    T_INT,      /* value in `number` */
    T_FLOAT,    /* 1.5, 2e10: as written, in `text` */
    T_CHAR,     /* character code in `number` */
    T_STRING,   /* contents in `text`, escapes replaced, as UTF-8 */
    T_EOF
};

typedef struct Token {
    enum TokenKind kind;
    const char *text;
    const char *qualifier; /* T_VARID, T_VARSYM: the M of M.x, or NULL */
    long number;
    int big;    /* T_INT: `number` holds the bits of a value from 2^63 to
                   2^64 - 1 */
    const char *file;
    int line;
    int col; /* 1-based; the layout rule depends on it */
} Token;

/* Lexes a whole file. The result always ends with a T_EOF token.
   `*extensions` receives the names in the file's LANGUAGE pragmas, ending
   with NULL. */
Token *lex(const char *file, const char *source, const char ***extensions);

int token_is(const Token *token, enum TokenKind kind, const char *text);
/* Whether the token is the ! of a bang pattern and not the operator: it has
   a space before it and none after it, as in `f !x = ...`. `previous` is
   the token before it. */
int token_is_bang(const Token *previous, const Token *token);
/* If the tokens from `start` are a clause that defines an operator between
   its two parameters, as in

       x <+> y = ...        or        x `plus` y = ...

   the position of the operator, or of its first backquote. Otherwise -1:
   for a clause with the name first, and for anything else. */
int infix_clause_operator(const Token *tokens, int start, int end);

#define TODO_AT(token, what) \
    todo((what), (token)->file, (token)->line, (token)->col, __FILE__, __LINE__)

#endif
