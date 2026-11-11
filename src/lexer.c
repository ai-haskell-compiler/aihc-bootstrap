#include "lexer.h"

#include <ctype.h>
#include <limits.h>
#include <string.h>

typedef struct Lexer {
    const char *file;
    const char *p; /* next unread character */
    int line;
    int col;
    Token *tokens;
    int count;
    int capacity;
    const char **extensions; /* from LANGUAGE pragmas; ends with NULL */
    int extension_count;
} Lexer;

#define LEX_TODO(lexer, what) \
    todo((what), (lexer)->file, (lexer)->line, (lexer)->col, __FILE__, __LINE__)

static const char *reserved_words[] = {
    "case", "class", "data", "default", "deriving", "do", "else", "foreign",
    "if", "import", "in", "infix", "infixl", "infixr", "instance", "let",
    "module", "newtype", "of", "then", "type", "where", "_", NULL
};

static const char *reserved_operators[] = {
    "..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>", NULL
};

static int is_one_of(const char *text, const char **list)
{
    int i;

    for (i = 0; list[i] != NULL; i++) {
        if (strcmp(text, list[i]) == 0) {
            return 1;
        }
    }
    return 0;
}

static int is_symbol(char c)
{
    return c != '\0' && strchr("!#$%&*+./<=>?@\\^|-~:", c) != NULL;
}

static int is_special(char c)
{
    return c != '\0' && strchr("()[],;`{}", c) != NULL;
}

static int is_identifier_char(char c)
{
    return isalnum((unsigned char)c) || c == '_' || c == '\'';
}

static void advance(Lexer *lexer)
{
    if (*lexer->p == '\n') {
        lexer->line++;
        lexer->col = 1;
    } else if (*lexer->p == '\t') {
        lexer->col += 8 - (lexer->col - 1) % 8;
    } else if (((unsigned char)*lexer->p & 0xC0) != 0x80) {
        lexer->col++; /* one column for each character, not each byte */
    }
    lexer->p++;
}

/* "--" starts a comment unless it is part of an operator such as "-->". */
static int at_line_comment(const Lexer *lexer)
{
    const char *q = lexer->p;

    if (q[0] != '-' || q[1] != '-') {
        return 0;
    }
    while (*q == '-') {
        q++;
    }
    return !is_symbol(*q);
}

static void add_extension(Lexer *lexer, const char *name)
{
    lexer->extensions = xrealloc(
        lexer->extensions,
        (size_t)(lexer->extension_count + 2) * sizeof(const char *));
    lexer->extensions[lexer->extension_count++] = name;
    lexer->extensions[lexer->extension_count] = NULL;
}

/* If the comment at the current position is {-# LANGUAGE A, B #-}, records
   A and B. Does not move. */
static void read_language_pragma(Lexer *lexer)
{
    const char *p = lexer->p + 3;

    while (isspace((unsigned char)*p)) {
        p++;
    }
    if (strncmp(p, "LANGUAGE", 8) != 0) {
        return;
    }
    p += 8;
    while (*p != '\0' && *p != '#') {
        const char *start = p;

        while (isalnum((unsigned char)*p)) {
            p++;
        }
        if (p > start) {
            add_extension(lexer, copy_string(start, (size_t)(p - start)));
        } else {
            p++;
        }
    }
}

/* Block comments nest. Pragmas are block comments too; only LANGUAGE
   pragmas are looked at. */
static void skip_block_comment(Lexer *lexer)
{
    int depth = 0;

    if (lexer->p[2] == '#') {
        read_language_pragma(lexer);
    }

    do {
        if (lexer->p[0] == '{' && lexer->p[1] == '-') {
            depth++;
            advance(lexer);
            advance(lexer);
        } else if (lexer->p[0] == '-' && lexer->p[1] == '}') {
            depth--;
            advance(lexer);
            advance(lexer);
        } else if (*lexer->p == '\0') {
            die("%s: unterminated comment", lexer->file);
        } else {
            advance(lexer);
        }
    } while (depth > 0);
}

static void skip_blanks_and_comments(Lexer *lexer)
{
    for (;;) {
        if (isspace((unsigned char)*lexer->p)) {
            advance(lexer);
        } else if (at_line_comment(lexer)) {
            while (*lexer->p != '\n' && *lexer->p != '\0') {
                advance(lexer);
            }
        } else if (lexer->p[0] == '{' && lexer->p[1] == '-') {
            skip_block_comment(lexer);
        } else if (*lexer->p == '#' && lexer->col == 1) {
            LEX_TODO(lexer, "CPP directive");
        } else {
            return;
        }
    }
}

/* Appends a token positioned at the next unread character. */
static Token *add_token(Lexer *lexer)
{
    Token *token;

    if (lexer->count == lexer->capacity) {
        lexer->capacity = lexer->capacity * 2 + 64;
        lexer->tokens = xrealloc(lexer->tokens,
                                 (size_t)lexer->capacity * sizeof(Token));
    }
    token = &lexer->tokens[lexer->count++];
    token->kind = T_EOF;
    token->text = "";
    token->qualifier = NULL;
    token->number = 0;
    token->big = 0;
    token->file = lexer->file;
    token->line = lexer->line;
    token->col = lexer->col;
    return token;
}

/* The names of the ASCII control characters, for escapes like \ESC. A name
   that starts another one comes after it: SOH before SO. */
static const char *ascii_names[] = {
    "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "BS", "HT", "LF",
    "VT", "FF", "CR", "SO", "SI", "DLE", "DC1", "DC2", "DC3", "DC4", "NAK",
    "SYN", "ETB", "CAN", "EM", "SUB", "ESC", "FS", "GS", "RS", "US", "SP",
    "DEL", NULL
};
static const int ascii_codes[] = {
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
    11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
    22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
    127
};

static int digit_value(char c)
{
    if (isdigit((unsigned char)c)) {
        return c - '0';
    }
    if (isxdigit((unsigned char)c)) {
        return tolower((unsigned char)c) - 'a' + 10;
    }
    return 99;
}

/* A number in the given base, as in the escapes \955, \x3bb and \o1673. */
static long lex_escape_number(Lexer *lexer, int base)
{
    long code = 0;

    while (digit_value(*lexer->p) < base) {
        code = code * base + digit_value(*lexer->p);
        advance(lexer);
    }
    return code;
}

/* What follows the backslash of an escape. Returns the character, or -1
   for the two escapes that stand for nothing: \& and a gap, which is
   whitespace between two backslashes. */
static long lex_escape(Lexer *lexer)
{
    char c = *lexer->p;
    int i;

    if (isdigit((unsigned char)c)) {
        return lex_escape_number(lexer, 10);
    }
    if (isspace((unsigned char)c)) {
        while (isspace((unsigned char)*lexer->p)) {
            advance(lexer);
        }
        advance(lexer); /* the closing backslash */
        return -1;
    }
    for (i = 0; ascii_names[i] != NULL; i++) {
        size_t length = strlen(ascii_names[i]);

        if (strncmp(lexer->p, ascii_names[i], length) == 0) {
            for (; length > 0; length--) {
                advance(lexer);
            }
            return ascii_codes[i];
        }
    }
    advance(lexer);
    switch (c) {
    case 'a': return 7;
    case 'b': return 8;
    case 'f': return 12;
    case 'n': return 10;
    case 'r': return 13;
    case 't': return 9;
    case 'v': return 11;
    case '\\': case '"': case '\'': return c;
    case '&': return -1;
    case 'x': case 'X': return lex_escape_number(lexer, 16);
    case 'o': case 'O': return lex_escape_number(lexer, 8);
    case '^':
        c = *lexer->p;
        advance(lexer);
        return c - '@'; /* \^A is 1 */
    default:
        LEX_TODO(lexer, "escape sequence");
    }
    return 0;
}

/* Reads one character of a character or string literal. Returns -1 for an
   escape that stands for nothing. */
static long lex_literal_char(Lexer *lexer)
{
    const char *start = lexer->p;
    const char *end = start;
    long code;

    if (*lexer->p == '\\') {
        advance(lexer);
        return lex_escape(lexer);
    }
    code = utf8_decode(&end);
    while (lexer->p < end) {
        advance(lexer);
    }
    return code;
}

static void lex_string(Lexer *lexer, Token *token)
{
    /* No character takes more bytes in UTF-8 than it does in the source,
       except \0, which takes the same two. */
    char *contents = xmalloc(strlen(lexer->p) + 4);
    size_t length = 0;

    advance(lexer);
    while (*lexer->p != '"') {
        long code;

        if (*lexer->p == '\0' || *lexer->p == '\n') {
            die("%s:%d: unterminated string", lexer->file, lexer->line);
        }
        code = lex_literal_char(lexer);
        if (code >= 0) {
            length += (size_t)utf8_encode(code, contents + length);
        }
    }
    advance(lexer);
    contents[length] = '\0';
    token->kind = T_STRING;
    token->text = contents;
}

static void lex_char(Lexer *lexer, Token *token)
{
    advance(lexer);
    token->kind = T_CHAR;
    token->number = lex_literal_char(lexer);
    if (*lexer->p != '\'') {
        LEX_TODO(lexer, "quote that is not a character literal");
    }
    advance(lexer);
}

/* 123, 0x7f, 0o17, 0b101. Underscores between digits are skipped. */
static void lex_number(Lexer *lexer, Token *token)
{
    const char *start = lexer->p;
    unsigned long value = 0;
    int base = 10;
    int too_big = 0; /* for an unsigned long */

    if (lexer->p[0] == '0' && lexer->p[1] != '\0'
        && strchr("xXoObB", lexer->p[1]) != NULL) {
        char letter = (char)tolower((unsigned char)lexer->p[1]);

        base = letter == 'x' ? 16 : letter == 'o' ? 8 : 2;
        advance(lexer);
        advance(lexer);
    }
    while (digit_value(*lexer->p) < base || *lexer->p == '_') {
        if (*lexer->p != '_') {
            unsigned long digit = (unsigned long)digit_value(*lexer->p);

            if (value > (ULONG_MAX - digit) / (unsigned long)base) {
                too_big = 1;
            }
            value = value * (unsigned long)base + digit;
        }
        advance(lexer);
    }
    if (base == 10
        && ((lexer->p[0] == '.' && isdigit((unsigned char)lexer->p[1]))
            || *lexer->p == 'e' || *lexer->p == 'E')) {
        /* A floating point literal: digits [. digits] [e [+-] digits] */
        if (*lexer->p == '.') {
            advance(lexer);
            while (isdigit((unsigned char)*lexer->p) || *lexer->p == '_') {
                advance(lexer);
            }
        }
        if (*lexer->p == 'e' || *lexer->p == 'E') {
            advance(lexer);
            if (*lexer->p == '+' || *lexer->p == '-') {
                advance(lexer);
            }
            while (isdigit((unsigned char)*lexer->p)) {
                advance(lexer);
            }
        }
        token->kind = T_FLOAT;
        token->text = copy_string(start, (size_t)(lexer->p - start));
        return;
    }
    token->kind = T_INT;
    token->number = (long)value;
    token->big = value > (unsigned long)LONG_MAX;
    if (too_big) {
        token->big = 2;
        token->text = copy_string(start, (size_t)(lexer->p - start));
    }
}

static void lex_operator(Lexer *lexer, Token *token)
{
    const char *start = lexer->p;

    while (is_symbol(*lexer->p)) {
        advance(lexer);
    }
    token->text = copy_string(start, (size_t)(lexer->p - start));
    if (is_one_of(token->text, reserved_operators)) {
        token->kind = T_RESERVED;
    } else {
        token->kind = T_VARSYM;
    }
}

static void lex_identifier(Lexer *lexer, Token *token)
{
    const char *start = lexer->p;
    int upper = isupper((unsigned char)*start);

    while (is_identifier_char(*lexer->p)) {
        advance(lexer);
    }
    /* Module names: Data.Map.Strict is a single T_CONID. */
    while (upper && lexer->p[0] == '.' && isupper((unsigned char)lexer->p[1])) {
        advance(lexer);
        while (is_identifier_char(*lexer->p)) {
            advance(lexer);
        }
    }
    /* M.x and M.+ are one token: the name, with M as its qualifier. */
    if (upper && lexer->p[0] == '.'
        && (islower((unsigned char)lexer->p[1]) || lexer->p[1] == '_')) {
        token->qualifier = copy_string(start, (size_t)(lexer->p - start));
        advance(lexer);
        start = lexer->p;
        while (is_identifier_char(*lexer->p)) {
            advance(lexer);
        }
        token->text = copy_string(start, (size_t)(lexer->p - start));
        token->kind = T_VARID;
        return;
    }
    if (upper && lexer->p[0] == '.' && is_symbol(lexer->p[1])) {
        token->qualifier = copy_string(start, (size_t)(lexer->p - start));
        advance(lexer);
        lex_operator(lexer, token);
        return;
    }
    token->text = copy_string(start, (size_t)(lexer->p - start));
    if (upper) {
        token->kind = T_CONID;
    } else if (is_one_of(token->text, reserved_words)) {
        token->kind = T_RESERVED;
    } else {
        token->kind = T_VARID;
    }
}

Token *lex(const char *file, const char *source, const char ***extensions)
{
    Lexer lexer;

    lexer.file = file;
    lexer.p = source;
    lexer.line = 1;
    lexer.col = 1;
    lexer.tokens = NULL;
    lexer.count = 0;
    lexer.capacity = 0;
    lexer.extensions = xmalloc(sizeof(const char *));
    lexer.extensions[0] = NULL;
    lexer.extension_count = 0;

    for (;;) {
        Token *token;
        char c;

        skip_blanks_and_comments(&lexer);
        token = add_token(&lexer);
        c = *lexer.p;
        if (c == '\0') {
            *extensions = lexer.extensions;
            return lexer.tokens; /* the token just added is the T_EOF */
        } else if (isdigit((unsigned char)c)) {
            lex_number(&lexer, token);
        } else if (c == '"') {
            lex_string(&lexer, token);
        } else if (c == '\'') {
            lex_char(&lexer, token);
        } else if (isalpha((unsigned char)c) || c == '_') {
            lex_identifier(&lexer, token);
        } else if (is_special(c)) {
            token->kind = T_SPECIAL;
            token->text = copy_string(lexer.p, 1);
            advance(&lexer);
        } else if (is_symbol(c)) {
            lex_operator(&lexer, token);
        } else {
            LEX_TODO(&lexer, "character the lexer does not know");
        }
    }
}

int token_is(const Token *token, enum TokenKind kind, const char *text)
{
    return token->kind == kind && strcmp(token->text, text) == 0;
}

int token_is_bang(const Token *previous, const Token *token)
{
    const Token *next = token + 1;

    return token_is(token, T_VARSYM, "!")
        && next->line == token->line && next->col == token->col + 1
        && (previous->line != token->line
            || previous->col + (int)strlen(previous->text) < token->col);
}

int infix_clause_operator(const Token *tokens, int start, int end)
{
    int depth = 0;
    int i;

    for (i = start; i < end; i++) {
        const Token *token = &tokens[i];

        if (token->kind == T_SPECIAL) {
            if (strchr("([{", token->text[0]) != NULL) {
                depth++;
            } else if (strchr(")]}", token->text[0]) != NULL) {
                depth--;
            } else if (token->text[0] == '`' && depth == 0) {
                return i > start ? i : -1;
            }
        } else if (depth == 0 && token->kind == T_RESERVED
                   && (strcmp(token->text, "=") == 0
                       || strcmp(token->text, "|") == 0)) {
            return -1; /* the parameters ended without an operator */
        } else if (depth == 0 && token->kind == T_VARSYM && i > start
                   && token->text[0] != ':'
                   && !token_is_bang(token - 1, token)
                   && !(strcmp(token->text, "-") == 0
                        && (token + 1)->kind == T_INT)) {
            return i;
        }
    }
    return -1;
}
