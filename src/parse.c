#include "syntax.h"

#include <stdio.h>
#include <string.h>

/* Parses the clauses of one top-level variable. This is a plain recursive
   descent parser over the tokens of a single declaration.

   Some convenience syntax is rewritten on the spot, so that the rest of the
   interpreter never sees it:
     f x = e                  f = \x -> e
     \(Just x) -> e           \v -> case v of Just x -> e
     f p = a; f q = b         f = \v -> case v of { p -> a; q -> b }
     if c then a else b       case c of { True -> a; False -> b }
     e where { decls }        let { decls } in e
     f p | g = a              f = \v -> case v of { p | g -> a }
     do { let decls; rest }   let { decls } in do { rest }
     let (a, b) = e in ...    let { v = e; a = case v of (a, b) -> a;
                                    b = case v of (a, b) -> b } in ...
     [a, b]                   a : b : []
     [a ..], [a, b .. c]      enumFrom a, enumFromThenTo a b c
     [e | p <- l, c]          concatMap (\v -> case v of
                                 p -> if c then [e] else []; _ -> []) l
     (+ a)                    \v -> v + a
     (a +)                    (+) a
     do { p <- e; rest }      e >>= \p -> do { rest }
     do { e; rest }           e >> do { rest }
   The names >>= and >> are the Prelude's, whatever is in scope. */

typedef struct Parser {
    const Token *tokens;
    int position;
    int end;        /* one past the last token of the declaration */
    int overloaded_strings;
    int indent;     /* column of the layout block we are in, or 0 */
    int item_start; /* position where the current item of that block began */
} Parser;

/* A layout block: the items after `of` or `do`. All items start in the same
   column. An item ends before the first token that starts a line at that
   column or to the left of it. */
typedef struct Block {
    int column; /* 0 for a block without items */
    int started;
    int outer_indent;
    int outer_item_start;
} Block;

/* One clause of a function: patterns = body */
typedef struct Clause {
    const Token *token;
    int count;
    Pattern **patterns;
    Alternative *rhs; /* everything after the patterns; its own pattern is
                         not set */
} Clause;

static Expr *parse_expression(Parser *parser);
static Expr *parse_operator(Parser *parser);
static Expr *parse_local_bindings(Parser *parser, const Token *token);
static void parse_rhs(Parser *parser, const char *separator,
                      Alternative *alternative);
static void finish_let(Expr *let, Expr *body);

static const Token *peek(const Parser *parser)
{
    return &parser->tokens[parser->position];
}

static int first_on_line(const Parser *parser, int position)
{
    return parser->tokens[position].line != parser->tokens[position - 1].line;
}

/* Whether the current item (or the whole declaration) has no more tokens. */
static int at_end(const Parser *parser)
{
    if (parser->position >= parser->end) {
        return 1;
    }
    if (parser->position == parser->item_start) {
        return 0;
    }
    if (peek(parser)->col == parser->indent
        && (token_is(peek(parser), T_RESERVED, "then")
            || token_is(peek(parser), T_RESERVED, "else"))) {
        /* In a do block, the `then` and `else` of an `if` may start a line
           in the column of the statements. */
        return 0;
    }
    return peek(parser)->col <= parser->indent
        && first_on_line(parser, parser->position);
}

static int next_is(const Parser *parser, enum TokenKind kind, const char *text)
{
    return !at_end(parser) && token_is(peek(parser), kind, text);
}

static int next_is_kind(const Parser *parser, enum TokenKind kind)
{
    return !at_end(parser) && peek(parser)->kind == kind;
}

static void expect(Parser *parser, enum TokenKind kind, const char *text,
                   const char *otherwise)
{
    if (!next_is(parser, kind, text)) {
        TODO_AT(peek(parser), otherwise);
    }
    parser->position++;
}

/* ---- Layout blocks ---- */

static void open_block(Parser *parser, Block *block)
{
    if (next_is(parser, T_SPECIAL, "{")) {
        TODO_AT(peek(parser), "block in explicit braces");
    }
    block->column = at_end(parser) ? 0 : peek(parser)->col;
    block->started = 0;
    block->outer_indent = parser->indent;
    block->outer_item_start = parser->item_start;
}

/* Moves to the next item of the block. Returns 0, and leaves the block, if
   there is none. */
static int next_item(Parser *parser, Block *block)
{
    int found;

    if (block->column == 0 || parser->position >= parser->end) {
        found = 0;
    } else if (!block->started) {
        found = 1;
    } else if (token_is(peek(parser), T_SPECIAL, ";")) {
        parser->position++;
        found = parser->position < parser->end;
    } else if (token_is(peek(parser), T_RESERVED, "where")
               || token_is(peek(parser), T_RESERVED, "then")
               || token_is(peek(parser), T_RESERVED, "else")) {
        /* These never start an item, whatever column they are in: they
           belong to something around the block. */
        found = 0;
    } else {
        found = peek(parser)->col == block->column
            && first_on_line(parser, parser->position);
    }
    block->started = 1;
    if (found) {
        parser->indent = block->column;
        parser->item_start = parser->position;
    } else {
        parser->indent = block->outer_indent;
        parser->item_start = block->outer_item_start;
    }
    return found;
}

/* ---- Building syntax ---- */

static Expr *new_expr(enum ExprKind kind, const Token *token)
{
    Expr *expr = xmalloc(sizeof(Expr));

    memset(expr, 0, sizeof(Expr));
    expr->kind = kind;
    expr->token = token;
    return expr;
}

static Expr *new_name(const Token *token, const char *name)
{
    Expr *expr = new_expr(E_NAME, token);

    expr->name = name;
    return expr;
}

/* A name that syntax stands for. */
static Expr *new_prelude_name(const Token *token, const char *name)
{
    Expr *expr = new_name(token, name);

    expr->qualifier = PRELUDE_NAME;
    return expr;
}

static Expr *new_constructor(const Token *token, const char *name)
{
    Expr *expr = new_expr(E_CONSTRUCTOR, token);

    expr->name = name;
    return expr;
}

static Expr *new_application(Expr *function, Expr *argument)
{
    Expr *expr = new_expr(E_APP, function->token);

    expr->function = function;
    expr->argument = argument;
    return expr;
}

static Pattern *new_pattern(enum PatternKind kind, const Token *token)
{
    Pattern *pattern = xmalloc(sizeof(Pattern));

    memset(pattern, 0, sizeof(Pattern));
    pattern->kind = kind;
    pattern->token = token;
    return pattern;
}

static Alternative *new_alternative(Pattern *pattern, Expr *body)
{
    Alternative *alternative = xmalloc(sizeof(Alternative));

    memset(alternative, 0, sizeof(Alternative));
    alternative->pattern = pattern;
    alternative->body = body;
    return alternative;
}

/* (a, b, c): the constructor (,,) applied to the elements. */
static Expr *new_tuple(const Token *token, Expr **elements, int count)
{
    Expr *expr = new_constructor(token, tuple_name(count));
    int i;

    for (i = 0; i < count; i++) {
        expr = new_application(expr, elements[i]);
    }
    return expr;
}

static Pattern *new_tuple_pattern(const Token *token, Pattern **elements,
                                  int count)
{
    Pattern *pattern = new_pattern(P_CONSTRUCTOR, token);

    pattern->name = tuple_name(count);
    pattern->argument_count = count;
    pattern->arguments = elements;
    return pattern;
}

/* A use of the local variable. */
static Expr *new_local(const Token *token, Binding *binding)
{
    Expr *expr = new_expr(E_LOCAL, token);

    expr->local = binding;
    return expr;
}

/* \pattern -> body. A lambda binds a single variable, so any other pattern
   becomes a case on a variable no source name can clash with. */
static Expr *new_lambda(const Token *token, Pattern *pattern, Expr *body)
{
    Expr *lambda = new_expr(E_LAMBDA, token);

    if (pattern->kind == P_VARIABLE) {
        lambda->local = pattern->variable;
        lambda->body = body;
    } else {
        Expr *variable = new_expr(E_LOCAL, token);

        lambda->local = new_binding("");
        variable->local = lambda->local;
        lambda->body = new_expr(E_CASE, token);
        lambda->body->scrutinee = variable;
        lambda->body->alternatives = new_alternative(pattern, body);
    }
    return lambda;
}

/* ---- Patterns ---- */

static int starts_pattern_atom(const Parser *parser)
{
    return next_is_kind(parser, T_VARID)
        || next_is_kind(parser, T_CONID)
        || next_is_kind(parser, T_INT)
        || next_is_kind(parser, T_CHAR)
        || next_is_kind(parser, T_STRING)
        || next_is(parser, T_RESERVED, "_")
        || next_is(parser, T_VARSYM, "!")
        || next_is(parser, T_SPECIAL, "(")
        || next_is(parser, T_SPECIAL, "[");
}

static Pattern *parse_pattern(Parser *parser);
static const char *operator_constructor_name(const Token *token);
static Expr *parse_expression(Parser *parser);

static Pattern *new_cons_pattern(const Token *token, Pattern *head,
                                 Pattern *tail)
{
    Pattern *cons = new_pattern(P_CONSTRUCTOR, token);

    cons->name = ":";
    cons->argument_count = 2;
    cons->arguments = xmalloc(2 * sizeof(Pattern *));
    cons->arguments[0] = head;
    cons->arguments[1] = tail;
    return cons;
}

static Pattern *new_string_pattern(const Token *token, const char *text)
{
    Pattern *pattern = new_pattern(P_CONSTRUCTOR, token);
    Pattern *character;

    if (*text == '\0') {
        pattern->name = "[]";
        return pattern;
    }
    character = new_pattern(P_CHAR, token);
    character->number = utf8_decode(&text);
    return new_cons_pattern(token, character, new_string_pattern(token, text));
}

/* What follows an opening square bracket: [a, b] is a : b : []. */
static Pattern *parse_list_pattern(Parser *parser, const Token *open)
{
    Pattern *nil = new_pattern(P_CONSTRUCTOR, open);
    Pattern *element;

    nil->name = "[]";
    if (next_is(parser, T_SPECIAL, "]")) {
        parser->position++;
        return nil;
    }
    element = parse_pattern(parser);
    if (next_is(parser, T_SPECIAL, ",")) {
        parser->position++;
        return new_cons_pattern(open, element, parse_list_pattern(parser, open));
    }
    expect(parser, T_SPECIAL, "]", "list pattern other than [a, b, c]");
    return new_cons_pattern(open, element, nil);
}

/* The fields of a record pattern, starting at the opening brace:
   C { f = pattern, g, .. } */
static void parse_record_pattern(Parser *parser, Pattern *pattern)
{
    pattern->kind = P_RECORD;
    parser->position++;
    while (!next_is(parser, T_SPECIAL, "}")) {
        size_t count = (size_t)pattern->field_count + 1;

        if (next_is(parser, T_RESERVED, "..")) {
            parser->position++;
            pattern->field_wildcard = 1;
        } else {
            if (!next_is_kind(parser, T_VARID)) {
                TODO_AT(peek(parser), "record pattern syntax");
            }
            pattern->field_names = xrealloc(pattern->field_names,
                                            count * sizeof(const char *));
            pattern->field_patterns = xrealloc(pattern->field_patterns,
                                               count * sizeof(Pattern *));
            pattern->field_names[pattern->field_count] = peek(parser)->text;
            pattern->field_patterns[pattern->field_count] = NULL;
            parser->position++;
            if (next_is(parser, T_RESERVED, "=")) {
                parser->position++;
                pattern->field_patterns[pattern->field_count] =
                    parse_pattern(parser);
            }
            pattern->field_count++;
        }
        if (next_is(parser, T_SPECIAL, ",")) {
            parser->position++;
        }
    }
    parser->position++;
}

/* A pattern that can be an argument without parentheses around it. */
/* Whether what follows, inside parentheses, is `function -> pattern`:
   there is a -> before the next comma or the closing parenthesis, outside
   any brackets. A
   function written as a lambda or a case would have a -> of its own; that
   is not expected here. */
static int is_view_pattern(const Parser *parser)
{
    int depth = 0;
    int i;

    for (i = parser->position; i < parser->end; i++) {
        const Token *token = &parser->tokens[i];

        if (token->kind == T_SPECIAL) {
            if (strchr("([{", token->text[0]) != NULL) {
                depth++;
            } else if (strchr(")]}", token->text[0]) != NULL) {
                depth--;
                if (depth < 0) {
                    return 0;
                }
            } else if (token->text[0] == ',' && depth == 0) {
                return 0;
            }
        } else if (depth == 0 && token_is(token, T_RESERVED, "->")) {
            return 1;
        }
    }
    return 0;
}

/* A pattern inside parentheses: a pattern, or function -> pattern. */
static Pattern *parse_view_or_pattern(Parser *parser)
{
    Pattern *pattern;

    if (!is_view_pattern(parser)) {
        return parse_pattern(parser);
    }
    pattern = new_pattern(P_VIEW, peek(parser));
    pattern->literal = parse_expression(parser);
    expect(parser, T_RESERVED, "->", "view pattern");
    pattern->inner = parse_pattern(parser);
    return pattern;
}

static Pattern *parse_pattern_atom(Parser *parser)
{
    const Token *token = peek(parser);
    Pattern *pattern = NULL;

    if (next_is_kind(parser, T_VARID)) {
        pattern = new_pattern(P_VARIABLE, token);
        pattern->variable = new_binding(token->text);
        parser->position++;
        if (next_is(parser, T_RESERVED, "@")) {
            /* variable@pattern */
            parser->position++;
            pattern->kind = P_AS;
            pattern->inner = parse_pattern_atom(parser);
        }
    } else if (next_is(parser, T_VARSYM, "!")) {
        parser->position++;
        pattern = new_pattern(P_BANG, token);
        pattern->inner = parse_pattern_atom(parser);
    } else if (next_is(parser, T_RESERVED, "_")) {
        pattern = new_pattern(P_WILDCARD, token);
        parser->position++;
    } else if (next_is_kind(parser, T_CONID)) {
        pattern = new_pattern(P_CONSTRUCTOR, token);
        pattern->name = token->text;
        parser->position++;
        if (next_is(parser, T_SPECIAL, "{")) {
            parse_record_pattern(parser, pattern);
        }
    } else if (next_is_kind(parser, T_CHAR)) {
        pattern = new_pattern(P_CHAR, token);
        pattern->number = token->number;
        parser->position++;
    } else if (next_is(parser, T_SPECIAL, "(")) {
        parser->position++;
        if (next_is(parser, T_SPECIAL, ")")) {
            pattern = new_pattern(P_CONSTRUCTOR, token);
            pattern->name = "()";
        } else {
            pattern = parse_view_or_pattern(parser);
            if (next_is(parser, T_SPECIAL, ",")) {
                Pattern **elements = xmalloc(MAX_TUPLE * sizeof(Pattern *));
                int count = 1;

                elements[0] = pattern;
                while (next_is(parser, T_SPECIAL, ",")) {
                    parser->position++;
                    if (count == MAX_TUPLE) {
                        TODO_AT(token, "tuple with more elements than MAX_TUPLE");
                    }
                    elements[count++] = parse_view_or_pattern(parser);
                }
                pattern = new_tuple_pattern(token, elements, count);
            }
        }
        expect(parser, T_SPECIAL, ")", "parenthesised pattern other than (pattern)");
    } else if (next_is_kind(parser, T_STRING)) {
        parser->position++;
        if (parser->overloaded_strings) {
            pattern = new_pattern(P_LITERAL, token);
            pattern->literal = new_expr(E_STRING, token);
            pattern->literal->string = token->text;
        } else {
            /* "ab" is the pattern 'a' : 'b' : [] */
            pattern = new_string_pattern(token, token->text);
        }
    } else if (next_is_kind(parser, T_INT)) {
        parser->position++;
        pattern = new_pattern(P_LITERAL, token);
        pattern->literal = new_expr(E_INT, token);
        pattern->literal->number = token->number;
        pattern->literal->big = token->big;
    } else if (next_is(parser, T_VARSYM, "-")
               && parser->tokens[parser->position + 1].kind == T_INT) {
        /* A negative number: -1 */
        Expr *number = new_expr(E_INT, token);

        number->number = parser->tokens[parser->position + 1].number;
        parser->position += 2;
        pattern = new_pattern(P_LITERAL, token);
        pattern->literal =
            new_application(new_prelude_name(token, "negate"), number);
    } else if (next_is(parser, T_SPECIAL, "[")) {
        parser->position++;
        pattern = parse_list_pattern(parser, token);
    } else {
        TODO_AT(token, "pattern syntax");
    }
    return pattern;
}

static Pattern *parse_pattern(Parser *parser)
{
    Pattern *pattern;

    if (next_is_kind(parser, T_CONID)) {
        /* A constructor with its arguments. */
        pattern = new_pattern(P_CONSTRUCTOR, peek(parser));
        pattern->name = peek(parser)->text;
        parser->position++;
        if (next_is(parser, T_SPECIAL, "{")) {
            parse_record_pattern(parser, pattern);
        }
        while (starts_pattern_atom(parser)) {
            pattern->arguments = xrealloc(
                pattern->arguments,
                (size_t)(pattern->argument_count + 1) * sizeof(Pattern *));
            pattern->arguments[pattern->argument_count++] =
                parse_pattern_atom(parser);
        }
    } else {
        pattern = parse_pattern_atom(parser);
    }
    if (next_is(parser, T_RESERVED, ":")) {
        /* head : tail, which groups to the right. */
        const Token *colon = peek(parser);

        parser->position++;
        return new_cons_pattern(colon, pattern, parse_pattern(parser));
    }
    if (next_is_kind(parser, T_VARSYM) && peek(parser)->text[0] == ':') {
        /* An infix constructor: a :| b. Like the list constructor, it is
           taken to group to the right. */
        Pattern *infix = new_pattern(P_CONSTRUCTOR, peek(parser));

        infix->name = operator_constructor_name(peek(parser));
        parser->position++;
        infix->argument_count = 2;
        infix->arguments = xmalloc(2 * sizeof(Pattern *));
        infix->arguments[0] = pattern;
        infix->arguments[1] = parse_pattern(parser);
        return infix;
    }
    return pattern;
}

/* ---- Expressions ---- */

static int starts_atom(const Parser *parser)
{
    return next_is_kind(parser, T_VARID)
        || next_is_kind(parser, T_CONID)
        || next_is_kind(parser, T_INT)
        || next_is_kind(parser, T_CHAR)
        || next_is_kind(parser, T_STRING)
        || next_is(parser, T_SPECIAL, "(")
        || next_is(parser, T_SPECIAL, "[");
}

/* An operator name is a constructor if it starts with a colon. */
/* The name of a constructor that is an operator, with its qualifier:
   NE.:| */
static const char *operator_constructor_name(const Token *token)
{
    char *name;

    if (token->qualifier == NULL) {
        return token->text;
    }
    name = xmalloc(strlen(token->qualifier) + strlen(token->text) + 2);
    sprintf(name, "%s.%s", token->qualifier, token->text);
    return name;
}

static Expr *new_operator(const Token *token)
{
    Expr *expr;

    if (token->text[0] == ':') {
        return new_constructor(token, operator_constructor_name(token));
    }
    expr = new_name(token, token->text);
    expr->qualifier = token->qualifier;
    return expr;
}

/* The rest of a tuple, from the comma after its first element. `first` is
   NULL if that element is left out. A tuple with elements left out is a
   function of them (a tuple section):

       (a, , c)  is  \v -> (a, v, c) */
static Expr *parse_tuple_rest(Parser *parser, const Token *open, Expr *first)
{
    Expr **elements = xmalloc(MAX_TUPLE * sizeof(Expr *));
    Binding *missing[MAX_TUPLE];
    int missing_count = 0;
    int count = 0;
    Expr *expr;

    for (;;) {
        if (count == MAX_TUPLE) {
            TODO_AT(open, "tuple with more elements than MAX_TUPLE");
        }
        if (count == 0 && first != NULL) {
            elements[count++] = first;
        } else if (next_is(parser, T_SPECIAL, ",")
                   || next_is(parser, T_SPECIAL, ")")) {
            missing[missing_count] = new_binding("");
            elements[count++] = new_local(open, missing[missing_count++]);
        } else {
            elements[count++] = parse_expression(parser);
        }
        if (!next_is(parser, T_SPECIAL, ",")) {
            break;
        }
        parser->position++;
    }
    expect(parser, T_SPECIAL, ")", "tuple syntax");
    expr = new_tuple(open, elements, count);
    while (missing_count > 0) {
        Expr *lambda = new_expr(E_LAMBDA, open);

        lambda->local = missing[--missing_count];
        lambda->body = expr;
        expr = lambda;
    }
    return expr;
}

/* What follows an opening parenthesis. */
static Expr *parse_parenthesised(Parser *parser, const Token *open)
{
    Expr *expr;

    if (next_is(parser, T_SPECIAL, ")")) {
        parser->position++;
        return new_constructor(open, "()");
    }
    if (next_is(parser, T_SPECIAL, ",")) {
        /* (,) and (,,): the tuple constructors as functions. */
        int start = parser->position;
        int count = 1;

        while (next_is(parser, T_SPECIAL, ",")) {
            parser->position++;
            count++;
        }
        if (next_is(parser, T_SPECIAL, ")")) {
            parser->position++;
            return new_constructor(open, tuple_name(count));
        }
        parser->position = start;
        return parse_tuple_rest(parser, open, NULL);
    }
    if ((next_is_kind(parser, T_VARSYM) || next_is(parser, T_RESERVED, ":"))
        && token_is(&parser->tokens[parser->position + 1], T_SPECIAL, ")")) {
        expr = new_operator(peek(parser)); /* (+) */
        parser->position += 2;
        return expr;
    }
    if ((next_is_kind(parser, T_VARSYM) && !next_is(parser, T_VARSYM, "-"))
        || next_is(parser, T_RESERVED, ":") || next_is(parser, T_SPECIAL, "`")) {
        /* A section with the operator first: (+ a) is \v -> v + a. */
        Expr *lambda = new_expr(E_LAMBDA, open);
        Expr *operator = parse_operator(parser);

        lambda->local = new_binding("");
        lambda->body = new_application(
            new_application(operator, new_local(open, lambda->local)),
            parse_expression(parser));
        expect(parser, T_SPECIAL, ")", "section syntax");
        return lambda;
    }
    expr = parse_expression(parser);
    if (next_is(parser, T_SPECIAL, ",")) {
        return parse_tuple_rest(parser, open, expr);
    }
    expect(parser, T_SPECIAL, ")", "parenthesised form other than (expression)");
    return expr;
}

static Expr *new_cons(const Token *token, Expr *head, Expr *tail)
{
    return new_application(
        new_application(new_constructor(token, ":"), head), tail);
}

/* The rest of a list after one of its elements: `, b, c]` or `]`. */
static Expr *parse_list_rest(Parser *parser, const Token *open)
{
    Expr *element;

    if (next_is(parser, T_SPECIAL, ",")) {
        parser->position++;
        element = parse_expression(parser);
        return new_cons(open, element, parse_list_rest(parser, open));
    }
    expect(parser, T_SPECIAL, "]", "list syntax");
    return new_constructor(open, "[]");
}

/* A Prelude function applied to up to three arguments (NULL for none). */
static Expr *new_prelude_call(const Token *token, const char *name, Expr *a,
                              Expr *b, Expr *c)
{
    Expr *expr = new_prelude_name(token, name);

    if (a != NULL) {
        expr = new_application(expr, a);
    }
    if (b != NULL) {
        expr = new_application(expr, b);
    }
    if (c != NULL) {
        expr = new_application(expr, c);
    }
    return expr;
}

/* The rest of a range after `..`: `]` or `c]`. `first` and `second` are
   what came before; `second` is NULL for [a ..]. */
static Expr *parse_range_rest(Parser *parser, const Token *open, Expr *first,
                              Expr *second)
{
    Expr *last = NULL;

    parser->position++;
    if (!next_is(parser, T_SPECIAL, "]")) {
        last = parse_expression(parser);
    }
    expect(parser, T_SPECIAL, "]", "range syntax");
    if (second == NULL) {
        return last == NULL
            ? new_prelude_call(open, "enumFrom", first, NULL, NULL)
            : new_prelude_call(open, "enumFromTo", first, last, NULL);
    }
    return last == NULL
        ? new_prelude_call(open, "enumFromThen", first, second, NULL)
        : new_prelude_call(open, "enumFromThenTo", first, second, last);
}

static int statement_is_bind(const Parser *parser);
static Alternative *new_bool_alternative(const Token *token, const char *name,
                                         Expr *body);

/* The qualifiers of a list comprehension and its closing bracket, with
   `element` as what the list is made of. Each qualifier wraps the rest:

       [e | p <- l, rest]     concatMap (\v -> case v of p -> [e | rest]
                                                         _ -> []) l
       [e | condition, rest]  if condition then [e | rest] else []
       [e | let decls, rest]  let decls in [e | rest]
       [e | ]                 [e] */
static Expr *parse_comprehension_rest(Parser *parser, const Token *open,
                                      Expr *element)
{
    const Token *token = peek(parser);
    Expr *rest;

    if (next_is(parser, T_SPECIAL, "]")) {
        parser->position++;
        return new_cons(open, element, new_constructor(open, "[]"));
    }
    if (next_is(parser, T_SPECIAL, ",")) {
        parser->position++;
        return parse_comprehension_rest(parser, open, element);
    }
    if (next_is(parser, T_RESERVED, "let")) {
        Expr *let;

        parser->position++;
        let = parse_local_bindings(parser, token);
        finish_let(let, parse_comprehension_rest(parser, open, element));
        return let;
    }
    if (statement_is_bind(parser)) {
        Pattern *pattern = parse_pattern(parser);
        Expr *lambda = new_expr(E_LAMBDA, token);
        Expr *match = new_expr(E_CASE, token);
        Expr *list;

        expect(parser, T_RESERVED, "<-", "list comprehension syntax");
        list = parse_expression(parser);
        rest = parse_comprehension_rest(parser, open, element);
        lambda->local = new_binding("");
        lambda->body = match;
        match->scrutinee = new_local(token, lambda->local);
        match->alternatives = new_alternative(pattern, rest);
        match->alternatives->next = new_alternative(
            new_pattern(P_WILDCARD, token), new_constructor(token, "[]"));
        return new_prelude_call(token, "concatMap", lambda, list, NULL);
    }
    {
        Expr *condition = parse_expression(parser);
        Expr *match = new_expr(E_CASE, token);

        rest = parse_comprehension_rest(parser, open, element);
        match->scrutinee = condition;
        match->alternatives = new_bool_alternative(token, "True", rest);
        match->alternatives->next =
            new_bool_alternative(token, "False", new_constructor(token, "[]"));
        return match;
    }
}

/* What follows an opening square bracket: a list, a range or a list
   comprehension. */
static Expr *parse_list(Parser *parser, const Token *open)
{
    Expr *first;
    Expr *second;

    if (next_is(parser, T_SPECIAL, "]")) {
        parser->position++;
        return new_constructor(open, "[]");
    }
    first = parse_expression(parser);
    if (next_is(parser, T_RESERVED, "..")) {
        return parse_range_rest(parser, open, first, NULL);
    }
    if (next_is(parser, T_RESERVED, "|")) {
        parser->position++;
        return parse_comprehension_rest(parser, open, first);
    }
    if (next_is(parser, T_SPECIAL, ",")) {
        parser->position++;
        second = parse_expression(parser);
        if (next_is(parser, T_RESERVED, "..")) {
            return parse_range_rest(parser, open, first, second);
        }
        return new_cons(open, first,
                        new_cons(open, second, parse_list_rest(parser, open)));
    }
    return new_cons(open, first, parse_list_rest(parser, open));
}

/* Record construction or update, starting at the opening brace:
   C { f = a, g, .. } or e { f = a }. `subject` is the C or the e. */
static Expr *parse_record(Parser *parser, Expr *subject)
{
    Expr *record = new_expr(E_RECORD, subject->token);

    record->function = subject;
    parser->position++;
    while (!next_is(parser, T_SPECIAL, "}")) {
        size_t count = (size_t)record->field_count + 1;

        if (next_is(parser, T_RESERVED, "..")) {
            parser->position++;
            record->field_wildcard = 1;
        } else {
            if (!next_is_kind(parser, T_VARID)) {
                TODO_AT(peek(parser), "record syntax");
            }
            record->field_names = xrealloc(record->field_names,
                                           count * sizeof(const char *));
            record->field_values = xrealloc(record->field_values,
                                            count * sizeof(Expr *));
            record->field_names[record->field_count] = peek(parser)->text;
            record->field_values[record->field_count] = NULL;
            if (record->field_count == 0) {
                record->field_qualifier = peek(parser)->qualifier;
            }
            parser->position++;
            if (next_is(parser, T_RESERVED, "=")) {
                parser->position++;
                record->field_values[record->field_count] =
                    parse_expression(parser);
            }
            record->field_count++;
        }
        if (next_is(parser, T_SPECIAL, ",")) {
            parser->position++;
        }
    }
    parser->position++;
    return record;
}

static Expr *parse_atom(Parser *parser)
{
    const Token *token = peek(parser);
    Expr *expr = NULL;

    if (next_is_kind(parser, T_VARID)) {
        expr = new_name(token, token->text);
        expr->qualifier = token->qualifier;
        parser->position++;
    } else if (next_is_kind(parser, T_CONID)) {
        expr = new_constructor(token, token->text);
        parser->position++;
    } else if (next_is_kind(parser, T_INT)) {
        expr = new_expr(E_INT, token);
        expr->number = token->number;
        expr->big = token->big;
        parser->position++;
    } else if (next_is_kind(parser, T_CHAR)) {
        expr = new_expr(E_CHAR, token);
        expr->number = token->number;
        parser->position++;
    } else if (next_is_kind(parser, T_STRING)) {
        expr = new_expr(E_STRING, token);
        expr->string = token->text;
        parser->position++;
    } else if (next_is(parser, T_SPECIAL, "(")) {
        parser->position++;
        expr = parse_parenthesised(parser, token);
    } else if (next_is(parser, T_SPECIAL, "[")) {
        parser->position++;
        expr = parse_list(parser, token);
    } else {
        TODO_AT(token, "expression syntax");
    }
    while (next_is(parser, T_SPECIAL, "{")) {
        expr = parse_record(parser, expr);
    }
    return expr;
}

static Expr *parse_application(Parser *parser)
{
    Expr *expr = parse_atom(parser);

    while (starts_atom(parser)) {
        expr = new_application(expr, parse_atom(parser));
    }
    return expr;
}

/* The part of a lambda after the backslash: x y -> body */
static void parse_alternatives(Parser *parser, Expr *match);

static Expr *parse_lambda_rest(Parser *parser)
{
    const Token *token = peek(parser);
    Pattern *pattern;

    if (next_is(parser, T_RESERVED, "case")) {
        /* \case alternatives  is  \v -> case v of alternatives */
        Expr *lambda = new_expr(E_LAMBDA, token);
        Expr *match = new_expr(E_CASE, token);

        lambda->local = new_binding("");
        match->scrutinee = new_local(token, lambda->local);
        parser->position++;
        parse_alternatives(parser, match);
        lambda->body = match;
        return lambda;
    }
    if (next_is(parser, T_RESERVED, "->")) {
        parser->position++;
        return parse_expression(parser);
    }
    pattern = parse_pattern_atom(parser);
    return new_lambda(token, pattern, parse_lambda_rest(parser));
}

/* An expression that may be followed by `where` and local bindings. */
static Expr *parse_body(Parser *parser)
{
    Expr *body = parse_expression(parser);

    if (next_is(parser, T_RESERVED, "where")) {
        const Token *token = peek(parser);
        Expr *let;

        parser->position++;
        let = parse_local_bindings(parser, token);
        finish_let(let, body);
        return let;
    }
    return body;
}

/* case scrutinee of { pattern -> body; ... } */
static Expr *parse_case(Parser *parser)
{
    Expr *expr = new_expr(E_CASE, peek(parser));

    parser->position++;
    expr->scrutinee = parse_expression(parser);
    expect(parser, T_RESERVED, "of", "case without of");
    parse_alternatives(parser, expr);
    return expr;
}

/* The block of alternatives of a case: pattern -> body */
static void parse_alternatives(Parser *parser, Expr *match)
{
    Alternative **last = &match->alternatives;
    Block block;

    open_block(parser, &block);
    while (next_item(parser, &block)) {
        Pattern *pattern = parse_pattern(parser);

        *last = new_alternative(pattern, NULL);
        parse_rhs(parser, "->", *last);
        last = &(*last)->next;
    }
}

/* Whether the statement or guard qualifier at the current position is
   `pattern <- expression`.
   We look for a <- that comes before anything a pattern cannot contain. */
static int statement_is_bind(const Parser *parser)
{
    static const char *stops[] = {
        "do", "case", "of", "if", "then", "else", "let", "in", "where", "\\",
        "=", "|", "->", NULL
    };
    int depth = 0;
    int i;
    int j;

    for (i = parser->position; i < parser->end; i++) {
        const Token *token = &parser->tokens[i];

        if (i > parser->position && token->col <= parser->indent
            && first_on_line(parser, i)) {
            return 0; /* the next statement */
        }
        if (token->kind == T_SPECIAL) {
            if (token->text[0] == '(' || token->text[0] == '['
                || token->text[0] == '{') {
                depth++;
            } else if (token->text[0] == ')' || token->text[0] == ']'
                       || token->text[0] == '}') {
                depth--;
                if (depth < 0) {
                    return 0; /* the end of what we are inside */
                }
            } else if ((token->text[0] == ';' || token->text[0] == ',')
                       && depth == 0) {
                return 0;
            }
        } else if (token->kind == T_RESERVED && depth == 0) {
            if (strcmp(token->text, "<-") == 0) {
                return 1;
            }
            for (j = 0; stops[j] != NULL; j++) {
                if (strcmp(token->text, stops[j]) == 0) {
                    return 0;
                }
            }
        }
    }
    return 0;
}

/* The statements of a do block, starting with the current one. */
static Expr *parse_statements(Parser *parser, Block *block)
{
    const Token *token = peek(parser);
    Pattern *pattern = NULL;
    Expr *expr;
    Expr *rest;

    if (next_is(parser, T_RESERVED, "let")) {
        Expr *let;

        parser->position++;
        let = parse_local_bindings(parser, token);
        if (next_is(parser, T_RESERVED, "in")) {
            /* An ordinary let expression used as a statement. */
            parser->position++;
            finish_let(let, parse_expression(parser));
            expr = let;
            if (!next_item(parser, block)) {
                return expr;
            }
            return new_application(
                new_application(new_prelude_name(token, ">>"), expr),
                parse_statements(parser, block));
        }
        if (!next_item(parser, block)) {
            die("%s:%d: do block ends with a let", token->file, token->line);
        }
        finish_let(let, parse_statements(parser, block));
        return let;
    }
    if (statement_is_bind(parser)) {
        pattern = parse_pattern(parser);
        expect(parser, T_RESERVED, "<-", "bind statement");
    }
    expr = parse_expression(parser);
    if (!next_item(parser, block)) {
        if (pattern != NULL) {
            die("%s:%d: do block ends with a bind", token->file, token->line);
        }
        return expr;
    }
    rest = parse_statements(parser, block);
    if (pattern != NULL) {
        return new_application(
            new_application(new_prelude_name(token, ">>="), expr),
            new_lambda(token, pattern, rest));
    }
    return new_application(
        new_application(new_prelude_name(token, ">>"), expr), rest);
}

static Expr *parse_do(Parser *parser)
{
    const Token *token = peek(parser);
    Block block;

    parser->position++;
    open_block(parser, &block);
    if (!next_item(parser, &block)) {
        die("%s:%d: empty do block", token->file, token->line);
    }
    return parse_statements(parser, &block);
}

static Alternative *new_bool_alternative(const Token *token, const char *name,
                                         Expr *body)
{
    Pattern *pattern = new_pattern(P_CONSTRUCTOR, token);

    pattern->qualifier = PRELUDE_NAME;
    pattern->name = name;
    return new_alternative(pattern, body);
}

/* if condition then a else b */
static Expr *parse_if(Parser *parser)
{
    const Token *token = peek(parser);
    Expr *expr = new_expr(E_CASE, token);
    Expr *yes;
    Expr *no;

    parser->position++;
    expr->scrutinee = parse_expression(parser);
    expect(parser, T_RESERVED, "then", "if without then");
    yes = parse_expression(parser);
    expect(parser, T_RESERVED, "else", "if without else");
    no = parse_expression(parser);
    expr->alternatives = new_bool_alternative(token, "True", yes);
    expr->alternatives->next = new_bool_alternative(token, "False", no);
    return expr;
}

/* One side of an operator. */
static Expr *parse_operand(Parser *parser)
{
    if (next_is(parser, T_RESERVED, "\\")) {
        parser->position++;
        return parse_lambda_rest(parser);
    }
    if (next_is(parser, T_RESERVED, "case")) {
        return parse_case(parser);
    }
    if (next_is(parser, T_RESERVED, "do")) {
        return parse_do(parser);
    }
    if (next_is(parser, T_RESERVED, "if")) {
        return parse_if(parser);
    }
    if (next_is(parser, T_RESERVED, "let")) {
        const Token *token = peek(parser);
        Expr *let;

        parser->position++;
        let = parse_local_bindings(parser, token);
        expect(parser, T_RESERVED, "in", "let without in");
        finish_let(let, parse_expression(parser));
        return let;
    }
    return parse_application(parser);
}

/* An infix operator: +, :, `div`. Returns NULL if the next token is not
   one. */
static Expr *parse_operator(Parser *parser)
{
    const Token *token = peek(parser);
    Expr *expr;

    if (next_is_kind(parser, T_VARSYM) || next_is(parser, T_RESERVED, ":")) {
        parser->position++;
        return new_operator(token);
    }
    if (next_is(parser, T_SPECIAL, "`")) {
        token = &parser->tokens[parser->position + 1];
        if (token->kind == T_CONID) {
            expr = new_constructor(token, token->text);
        } else {
            expr = new_name(token, token->text);
            expr->qualifier = token->qualifier;
        }
        parser->position += 3;
        return expr;
    }
    return NULL;
}

/* Moves past the type of `expression :: type`. The type pass parses it. */
static void skip_type(Parser *parser)
{
    static const char *stops[] = {
        "then", "else", "of", "in", "where", "=", "|", "<-", "..", NULL
    };
    int depth = 0;
    int j;

    while (!at_end(parser)) {
        const Token *token = peek(parser);

        if (token->kind == T_SPECIAL) {
            if (token->text[0] == '(' || token->text[0] == '[') {
                depth++;
            } else if (token->text[0] == ')' || token->text[0] == ']') {
                if (depth == 0) {
                    return;
                }
                depth--;
            } else if (depth == 0) {
                return; /* a comma or semicolon */
            }
        } else if (token->kind == T_RESERVED && depth == 0) {
            for (j = 0; stops[j] != NULL; j++) {
                if (strcmp(token->text, stops[j]) == 0) {
                    return;
                }
            }
        }
        parser->position++;
    }
}

/* Whether there is a minus sign in front of the next operand. Moves past
   it. */
static int parse_minus(Parser *parser)
{
    if (next_is(parser, T_VARSYM, "-")) {
        parser->position++;
        return 1;
    }
    return 0;
}

/* An E_OPERATORS with a single operand and nothing else is that operand. */
static Expr *simplify_operators(Expr *expr)
{
    if (expr->operator_count == 0 && !expr->negated[0]) {
        return expr->operands[0];
    }
    return expr;
}

/* operand operator operand ... [:: type]
   The operators are kept in a flat list. The resolver gives them their
   precedence, because that is a property of what the names refer to. */
static Expr *parse_expression(Parser *parser)
{
    Expr *expr = new_expr(E_OPERATORS, peek(parser));
    Expr *operator;

    expr->operands = xmalloc(sizeof(Expr *));
    expr->negated = xmalloc(sizeof(int));
    expr->negated[0] = parse_minus(parser);
    expr->operands[0] = parse_operand(parser);
    while ((operator = parse_operator(parser)) != NULL) {
        int count = expr->operator_count + 1;

        if (next_is(parser, T_SPECIAL, ")")) {
            /* A section with the operator last: (a +) is (+) a. */
            return new_application(operator, simplify_operators(expr));
        }
        expr->operators = xrealloc(expr->operators,
                                   (size_t)count * sizeof(Expr *));
        expr->operands = xrealloc(expr->operands,
                                  (size_t)(count + 1) * sizeof(Expr *));
        expr->negated = xrealloc(expr->negated,
                                 (size_t)(count + 1) * sizeof(int));
        expr->operators[count - 1] = operator;
        expr->negated[count] = parse_minus(parser);
        expr->operands[count] = parse_operand(parser);
        expr->operator_count = count;
    }
    expr = simplify_operators(expr);
    if (next_is(parser, T_RESERVED, "::")) {
        Expr *annotation = new_expr(E_ANNOTATION, expr->token);

        parser->position++;
        annotation->body = expr;
        annotation->type_start = parser->position;
        skip_type(parser);
        annotation->type_end = parser->position;
        expr = annotation;
    }
    return expr;
}

/* ---- Clauses and local bindings ---- */

/* The part of a clause or case alternative after its patterns:

       = body                               (the separator is = or ->)
       | guard = body | guard = body
   and then optionally `where` and local bindings. */
static void parse_rhs(Parser *parser, const char *separator,
                      Alternative *alternative)
{
    Guard **last_guard = &alternative->guards;

    if (!next_is(parser, T_RESERVED, "|")) {
        expect(parser, T_RESERVED, separator, "definition syntax");
        alternative->body = parse_expression(parser);
    }
    while (next_is(parser, T_RESERVED, "|")) {
        Guard *guard = xmalloc(sizeof(Guard));
        Qualifier **last = &guard->qualifiers;

        memset(guard, 0, sizeof(Guard));
        do {
            Qualifier *qualifier = xmalloc(sizeof(Qualifier));

            parser->position++; /* the | or the comma */
            memset(qualifier, 0, sizeof(Qualifier));
            if (next_is(parser, T_RESERVED, "let")) {
                /* let pattern = expression: as pattern <- expression, with
                   a pattern that always matches. */
                parser->position++;
                qualifier->pattern = parse_pattern(parser);
                expect(parser, T_RESERVED, "=",
                       "let in a guard other than let pattern = expression");
            } else if (statement_is_bind(parser)) {
                qualifier->pattern = parse_pattern(parser);
                expect(parser, T_RESERVED, "<-", "pattern guard");
            }
            qualifier->expr = parse_expression(parser);
            *last = qualifier;
            last = &qualifier->next;
        } while (next_is(parser, T_SPECIAL, ","));
        expect(parser, T_RESERVED, separator, "guard syntax");
        guard->body = parse_expression(parser);
        *last_guard = guard;
        last_guard = &guard->next;
    }
    if (next_is(parser, T_RESERVED, "where")) {
        const Token *token = peek(parser);

        parser->position++;
        alternative->where = parse_local_bindings(parser, token);
        finish_let(alternative->where, NULL);
    }
}

/* A clause, from after its name. If the clause defines an operator between
   its two parameters, `operator` is the position of the operator and the
   clause is read from its start; otherwise `operator` is negative. */
static void parse_clause(Parser *parser, Clause *clause, int operator)
{
    clause->token = peek(parser);
    clause->count = 0;
    clause->patterns = NULL;
    if (operator >= 0) {
        clause->patterns = xmalloc(2 * sizeof(Pattern *));
        clause->patterns[0] = parse_pattern(parser);
        if (parser->position != operator) {
            TODO_AT(peek(parser), "operator defined infix");
        }
        /* The operator is one token, or three as `name`. */
        parser->position += peek(parser)->kind == T_VARSYM ? 1 : 3;
        clause->patterns[1] = parse_pattern(parser);
        clause->count = 2;
    }
    while (!next_is(parser, T_RESERVED, "=")
           && !next_is(parser, T_RESERVED, "|")) {
        clause->patterns = xrealloc(
            clause->patterns, (size_t)(clause->count + 1) * sizeof(Pattern *));
        clause->patterns[clause->count++] = parse_pattern_atom(parser);
    }
    clause->rhs = new_alternative(NULL, NULL);
    parse_rhs(parser, "=", clause->rhs);
}

/* Several clauses, or one with guards, become one function that tries
   them in order:

       f p1 p2 = a          f = \v1 v2 -> case (v1, v2) of
       f q1 q2 | g = b                       (p1, p2) -> a
                                            (q1, q2) | g -> b

   A definition without parameters is a case on the unit value. */
static Expr *combine_clauses(const Clause *clauses, int count)
{
    const Token *token = clauses[0].token;
    int arity = clauses[0].count;
    Binding **variables = xmalloc((size_t)arity * sizeof(Binding *) + 1);
    Expr **locals = xmalloc((size_t)arity * sizeof(Expr *) + 1);
    Expr *expr = new_expr(E_CASE, token);
    Alternative **last = &expr->alternatives;
    int i;

    for (i = 0; i < arity; i++) {
        variables[i] = new_binding("");
        locals[i] = new_local(token, variables[i]);
    }
    if (arity == 0) {
        expr->scrutinee = new_constructor(token, "()");
    } else if (arity == 1) {
        expr->scrutinee = locals[0];
    } else {
        expr->scrutinee = new_tuple(token, locals, arity);
    }
    for (i = 0; i < count; i++) {
        Alternative *alternative = clauses[i].rhs;

        if (arity == 0) {
            alternative->pattern = new_pattern(P_CONSTRUCTOR, clauses[i].token);
            alternative->pattern->name = "()";
        } else if (arity == 1) {
            alternative->pattern = clauses[i].patterns[0];
        } else {
            alternative->pattern = new_tuple_pattern(
                clauses[i].token, clauses[i].patterns, arity);
        }
        *last = alternative;
        last = &alternative->next;
    }
    for (i = arity - 1; i >= 0; i--) {
        Expr *lambda = new_expr(E_LAMBDA, token);

        lambda->local = variables[i];
        lambda->body = expr;
        expr = lambda;
    }
    return expr;
}

/* The definition of a variable from all its clauses. */
static Expr *clauses_to_expr(const Clause *clauses, int count)
{
    Expr *expr;
    int i;

    if (count > 1 || clauses[0].rhs->guards != NULL) {
        return combine_clauses(clauses, count);
    }
    /* The common case stays simple: f x = e is \x -> e. */
    expr = clauses[0].rhs->body;
    if (clauses[0].rhs->where != NULL) {
        clauses[0].rhs->where->body = expr;
        expr = clauses[0].rhs->where;
    }
    for (i = clauses[0].count - 1; i >= 0; i--) {
        expr = new_lambda(clauses[0].token, clauses[0].patterns[i], expr);
    }
    return expr;
}

/* What is known about one local variable while its let is being parsed. */
typedef struct Local {
    const Token *token;
    const char *name;
    Clause *clauses;
    int clause_count;
    int signature_start;
    int signature_end;
} Local;

static Local *find_or_add_local(Local **locals, int *count, const Token *token,
                                const char *name)
{
    Local *local;
    int i;

    for (i = 0; i < *count; i++) {
        if (strcmp((*locals)[i].name, name) == 0) {
            return &(*locals)[i];
        }
    }
    *locals = xrealloc(*locals, (size_t)(*count + 1) * sizeof(Local));
    local = &(*locals)[(*count)++];
    memset(local, 0, sizeof(Local));
    local->token = token;
    local->name = name;
    return local;
}

static void add_let_binding(Expr *let, Binding *binding, Expr *value)
{
    size_t count = (size_t)let->binding_count + 1;

    let->bindings = xrealloc(let->bindings, count * sizeof(Binding *));
    let->values = xrealloc(let->values, count * sizeof(Expr *));
    let->bindings[let->binding_count] = binding;
    let->values[let->binding_count] = value;
    let->binding_count++;
}

/* Adds the variables of the pattern to the let: for each variable x,
   x = case value of pattern -> x. */
static void add_pattern_variables(Expr *let, const Pattern *whole,
                                  const Pattern *pattern, Binding *value)
{
    int i;

    if (pattern->kind == P_VARIABLE) {
        Expr *select = new_expr(E_CASE, pattern->token);

        select->scrutinee = new_local(pattern->token, value);
        select->alternatives = new_alternative(
            (Pattern *)whole, new_local(pattern->token, pattern->variable));
        add_let_binding(let, new_binding(pattern->variable->name), select);
    }
    if (pattern->kind == P_AS) {
        Expr *select = new_expr(E_CASE, pattern->token);

        select->scrutinee = new_local(pattern->token, value);
        select->alternatives = new_alternative(
            (Pattern *)whole, new_local(pattern->token, pattern->variable));
        add_let_binding(let, new_binding(pattern->variable->name), select);
    }
    if (pattern->inner != NULL) {
        add_pattern_variables(let, whole, pattern->inner, value);
    }
    for (i = 0; i < pattern->argument_count; i++) {
        add_pattern_variables(let, whole, pattern->arguments[i], value);
    }
}

/* If the item at the current position is a type signature `f, g :: type`,
   the position of its `::`. Otherwise -1. */
static int local_signature_colon(const Parser *parser)
{
    int i = parser->position;

    while (i < parser->end && parser->tokens[i].kind == T_VARID) {
        i++;
        if (i < parser->end && token_is(&parser->tokens[i], T_RESERVED, "::")) {
            return i;
        }
        if (i >= parser->end || !token_is(&parser->tokens[i], T_SPECIAL, ",")) {
            return -1;
        }
        i++;
    }
    return -1;
}

/* The bindings after `let` or `where`, as an E_LET without a body. */
static Expr *parse_local_bindings(Parser *parser, const Token *token)
{
    Expr *let = new_expr(E_LET, token);
    Local *locals = NULL;
    int count = 0;
    Block block;
    int i;

    open_block(parser, &block);
    while (next_item(parser, &block)) {
        const Token *first = peek(parser);
        const Token *second = &parser->tokens[parser->position + 1];
        int colon = local_signature_colon(parser);
        int infix_operator = colon >= 0 ? -1
            : infix_clause_operator(parser->tokens, parser->position,
                                    parser->end);

        if (colon >= 0) {
            /* f, g :: type */
            int start = colon + 1;

            for (i = parser->position; i < colon; i++) {
                if (parser->tokens[i].kind == T_VARID) {
                    Local *local = find_or_add_local(
                        &locals, &count, first, parser->tokens[i].text);

                    local->signature_start = start;
                }
            }
            parser->position = start;
            while (!at_end(parser)) {
                parser->position++;
            }
            for (i = 0; i < count; i++) {
                if (locals[i].signature_start == start) {
                    locals[i].signature_end = parser->position;
                }
            }
        } else if (infix_operator >= 0
                   && !(first->kind == T_VARID
                        && (second->kind != T_VARSYM
                            || token_is_bang(first, second))
                        && !token_is(second, T_SPECIAL, "`"))) {
            /* parameter + parameter = body */
            const Token *name = &parser->tokens[infix_operator];
            Local *local;

            if (name->kind != T_VARSYM) {
                name++; /* past the backquote */
            }
            local = find_or_add_local(&locals, &count, name, name->text);
            local->clauses = xrealloc(
                local->clauses,
                (size_t)(local->clause_count + 1) * sizeof(Clause));
            parse_clause(parser, &local->clauses[local->clause_count++],
                         infix_operator);
        } else if (first->kind == T_VARID
                   && !token_is(second, T_RESERVED, ":")
                   && !token_is(second, T_RESERVED, "@")) {
            /* f patterns = body */
            Local *local = find_or_add_local(&locals, &count, first, first->text);

            parser->position++;
            local->clauses = xrealloc(
                local->clauses,
                (size_t)(local->clause_count + 1) * sizeof(Clause));
            parse_clause(parser, &local->clauses[local->clause_count++], -1);
        } else {
            /* pattern = body */
            Pattern *pattern = parse_pattern(parser);
            Binding *value = new_binding("");

            expect(parser, T_RESERVED, "=", "local binding");
            add_let_binding(let, value, parse_body(parser));
            add_pattern_variables(let, pattern, pattern, value);
        }
    }
    for (i = 0; i < count; i++) {
        Binding *binding = new_binding(locals[i].name);

        if (locals[i].clause_count == 0) {
            die("%s:%d: %s has a type signature and no definition",
                locals[i].token->file, locals[i].token->line, locals[i].name);
        }
        binding->is_function = locals[i].clauses[0].count > 0;
        binding->signature_start = locals[i].signature_start;
        binding->signature_end = locals[i].signature_end;
        add_let_binding(let, binding, clauses_to_expr(locals[i].clauses,
                                                      locals[i].clause_count));
    }
    return let;
}

/* Gives the let its body, and each binding the place of its definition. */
static void finish_let(Expr *let, Expr *body)
{
    int i;

    let->body = body;
    for (i = 0; i < let->binding_count; i++) {
        let->bindings[i]->definition = &let->values[i];
    }
}

/* ---- Pattern synonyms ---- */

static void start_synonym_parser(Parser *parser, const PatternSynonym *synonym)
{
    parser->tokens = synonym->module->tokens;
    parser->overloaded_strings = synonym->module->overloaded_strings;
    parser->position = synonym->rhs_start;
    parser->end = synonym->rhs_end;
    parser->indent = 0;
    parser->item_start = synonym->rhs_start;
}

Pattern *parse_synonym_pattern(const PatternSynonym *synonym)
{
    Parser parser;
    Pattern *pattern;

    start_synonym_parser(&parser, synonym);
    pattern = parse_pattern(&parser);
    if (parser.position < parser.end) {
        TODO_AT(peek(&parser), "pattern syntax");
    }
    return pattern;
}

Expr *parse_synonym_selector(const PatternSynonym *synonym, const char *field)
{
    const Token *token = &synonym->module->tokens[synonym->rhs_start];
    Expr *lambda = new_expr(E_LAMBDA, token);
    Expr *match = new_expr(E_CASE, token);

    lambda->local = new_binding("");
    lambda->body = match;
    match->scrutinee = new_local(token, lambda->local);
    match->alternatives = new_alternative(parse_synonym_pattern(synonym),
                                          new_name(token, field));
    return lambda;
}

/* ---- Top-level variables ---- */

Expr *parse_global(Global *global)
{
    const Token *tokens = global->module->tokens;
    const Chunk *chunk;
    Clause *clauses = NULL;
    int count = 0;

    if (global->clauses == NULL) {
        die("%s.%s has no definition", global->module->name, global->name);
    }
    for (chunk = global->clauses; chunk != NULL; chunk = chunk->next) {
        const Token *first = &tokens[chunk->start];
        const Token *second = first + 1;
        int operator = -1;
        Parser parser;

        parser.tokens = tokens;
        parser.overloaded_strings = global->module->overloaded_strings;
        parser.end = chunk->end;
        parser.indent = 0;
        parser.item_start = chunk->start;
        parser.position = chunk->start;
        if (first->kind == T_VARID
            && (second->kind != T_VARSYM || token_is_bang(first, second))
            && !token_is(second, T_SPECIAL, "`")) {
            parser.position += 1; /* skip the name: f */
        } else if (first->kind == T_CONID
                   && strcmp(first->text, global->name) == 0) {
            parser.position += 1; /* a pattern synonym as a function: P */
        } else if (token_is(first, T_SPECIAL, "(") && second->kind == T_VARSYM
                   && token_is(second + 1, T_SPECIAL, ")")) {
            parser.position += 3; /* skip the name: (+) */
        } else {
            operator = infix_clause_operator(tokens, chunk->start, chunk->end);
        }
        clauses = xrealloc(clauses, (size_t)(count + 1) * sizeof(Clause));
        parse_clause(&parser, &clauses[count++], operator);
        if (parser.position < parser.end) {
            TODO_AT(&tokens[parser.position], "expression syntax");
        }
    }
    return clauses_to_expr(clauses, count);
}
