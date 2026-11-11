#include "eval.h"

#include <limits.h>
#include <stdio.h>
#include <string.h>

/* Arithmetic. Every integral type is a C long. Which type a builtin works
   at is its first argument, as for every polymorphic builtin, so one
   builtin serves all of them:

       numAdd :: a -> a -> a        at Int, Word8, Integer, ...

   A result is brought back into the range of its type: Word8 wraps at 256,
   Int wraps like a C long, as in GHC.

   Integer is a C long too while it fits, and otherwise a big number
   (integer.c). The operations here try the long first. */

/* The interpreter assumes a 64-bit long. */
typedef char long_must_be_64_bits[sizeof(long) == 8 ? 1 : -1];

enum NumberKind {
    K_INT,     /* Int, Int64 */
    K_INTEGER,
    K_WORD64,  /* Word, Word64 */
    K_WORD32,
    K_WORD16,
    K_WORD8,
    K_INT32,
    K_INT16,
    K_INT8,
    K_CHAR
};

static enum NumberKind number_kind(Value *type_value)
{
    static const char *names[] = {
        "Int", "Int64", "Integer", "Word", "Word64", "Word32", "Word16",
        "Word8", "Int32", "Int16", "Int8", "Char", NULL
    };
    static const enum NumberKind kinds[] = {
        K_INT, K_INT, K_INTEGER, K_WORD64, K_WORD64, K_WORD32, K_WORD16,
        K_WORD8, K_INT32, K_INT16, K_INT8, K_CHAR
    };
    Type *type = prune(force(type_value)->type);
    int i;

    if (type->kind == TY_CON) {
        for (i = 0; names[i] != NULL; i++) {
            if (strcmp(type->constructor->name, names[i]) == 0) {
                return kinds[i];
            }
        }
    }
    if (!has_failure_handler()) {
        fprintf(stderr, "arithmetic at the type ");
        print_type(type);
        fprintf(stderr, "\n");
    }
    TODO("arithmetic at a type that is not a built-in number type");
    return K_INT;
}

/* The bits brought into the range of the kind. */
static long wrap(enum NumberKind kind, unsigned long bits)
{
    switch (kind) {
    case K_WORD8:
        return (long)(bits & 0xFFul);
    case K_WORD16:
        return (long)(bits & 0xFFFFul);
    case K_WORD32:
        return (long)(bits & 0xFFFFFFFFul);
    case K_INT8:
        return (long)((bits & 0xFFul) ^ 0x80ul) - 0x80l;
    case K_INT16:
        return (long)((bits & 0xFFFFul) ^ 0x8000ul) - 0x8000l;
    case K_INT32:
        return (long)((bits & 0xFFFFFFFFul) ^ 0x80000000ul) - 0x80000000l;
    case K_INT:
    case K_INTEGER:
    case K_WORD64:
    case K_CHAR:
        break;
    }
    return (long)bits;
}

static Value *make_number(enum NumberKind kind, unsigned long bits)
{
    if (kind == K_CHAR) {
        return make_char(wrap(kind, bits));
    }
    return make_int(wrap(kind, bits));
}

/* The argument as a number that arithmetic can be done on. Not for an
   Integer that may be big. */
static long operand(enum NumberKind kind, Value *value)
{
    (void)kind;
    return force(value)->number;
}

/* Whether either Integer is big. */
static int any_big(Value *a, Value *b)
{
    return integer_is_big(force(a)) || integer_is_big(force(b));
}

/* a + b, a - b or a * b in *result, if it fits in a long. */
static int fits(char operation, long a, long b, long *result)
{
    switch (operation) {
    case '+':
        if ((b > 0 && a > LONG_MAX - b) || (b < 0 && a < LONG_MIN - b)) {
            return 0;
        }
        *result = a + b;
        return 1;
    case '-':
        if ((b < 0 && a > LONG_MAX + b) || (b > 0 && a < LONG_MIN + b)) {
            return 0;
        }
        *result = a - b;
        return 1;
    default:
        if (a != 0 && b != 0) {
            if ((a == -1 && b == LONG_MIN) || (b == -1 && a == LONG_MIN)) {
                return 0;
            }
            if (a > 0 ? (b > 0 ? a > LONG_MAX / b : b < LONG_MIN / a)
                      : (b > 0 ? a < LONG_MIN / b : b < LONG_MAX / a)) {
                return 0;
            }
        }
        *result = a * b;
        return 1;
    }
}

/* numAdd, numSub, numMul :: a -> a -> a */
static Value *arithmetic(char operation, Value **arguments)
{
    enum NumberKind kind = number_kind(arguments[0]);
    unsigned long x;
    unsigned long y;

    if (kind == K_INTEGER) {
        long result;

        if (!any_big(arguments[1], arguments[2])
            && fits(operation, force(arguments[1])->number,
                    force(arguments[2])->number, &result)) {
            return make_int(result);
        }
        return integer_arithmetic(operation, arguments[1], arguments[2]);
    }
    x = (unsigned long)operand(kind, arguments[1]);
    y = (unsigned long)operand(kind, arguments[2]);
    /* Unsigned arithmetic wraps, which is what every other type wants. */
    switch (operation) {
    case '+':
        return make_number(kind, x + y);
    case '-':
        return make_number(kind, x - y);
    default:
        return make_number(kind, x * y);
    }
}

Value *builtin_numAdd(Value **arguments)
{
    return arithmetic('+', arguments);
}

Value *builtin_numSub(Value **arguments)
{
    return arithmetic('-', arguments);
}

Value *builtin_numMul(Value **arguments)
{
    return arithmetic('*', arguments);
}

/* numQuot, numRem, numDiv, numMod :: a -> a -> a
   quot and rem round towards zero; div and mod round down. */
static Value *division(char operation, Value **arguments)
{
    enum NumberKind kind = number_kind(arguments[0]);
    long a;
    long b;
    long quotient;
    long remainder;

    if (kind == K_INTEGER && any_big(arguments[1], arguments[2])) {
        return integer_division(operation, arguments[1], arguments[2]);
    }
    a = operand(kind, arguments[1]);
    b = operand(kind, arguments[2]);
    if (b == 0) {
        TODO("exception for division by zero");
    }
    if (kind == K_WORD64) {
        unsigned long x = (unsigned long)a;
        unsigned long y = (unsigned long)b;

        return make_int((long)(operation == 'q' || operation == 'd' ? x / y
                                                                     : x % y));
    }
    if (a == LONG_MIN && b == -1) {
        if (kind == K_INTEGER) {
            return integer_division(operation, arguments[1], arguments[2]);
        }
        return make_number(kind, operation == 'q' || operation == 'd'
                                     ? (unsigned long)a : 0ul);
    }
    /* C89 leaves the rounding of / open for negative numbers, so start from
       the absolute values. */
    {
        unsigned long x = a < 0 ? 0ul - (unsigned long)a : (unsigned long)a;
        unsigned long y = b < 0 ? 0ul - (unsigned long)b : (unsigned long)b;

        quotient = (long)(x / y);
        remainder = (long)(x % y);
        if ((a < 0) != (b < 0)) {
            quotient = -quotient;
        }
        if (a < 0) {
            remainder = -remainder;
        }
    }
    if ((operation == 'd' || operation == 'm') && remainder != 0
        && (remainder < 0) != (b < 0)) {
        quotient -= 1;
        remainder += b;
    }
    return make_number(kind, (unsigned long)(operation == 'q' || operation == 'd'
                                                 ? quotient : remainder));
}

Value *builtin_numQuot(Value **arguments)
{
    return division('q', arguments);
}

Value *builtin_numRem(Value **arguments)
{
    return division('r', arguments);
}

Value *builtin_numDiv(Value **arguments)
{
    return division('d', arguments);
}

Value *builtin_numMod(Value **arguments)
{
    return division('m', arguments);
}

/* numEq :: a -> a -> Bool */
Value *builtin_numEq(Value **arguments)
{
    enum NumberKind kind = number_kind(arguments[0]);

    if (kind == K_INTEGER && any_big(arguments[1], arguments[2])) {
        return make_bool(integer_compare(arguments[1], arguments[2]) == 0);
    }
    return make_bool(operand(kind, arguments[1]) == operand(kind, arguments[2]));
}

/* numLess :: a -> a -> Bool */
Value *builtin_numLess(Value **arguments)
{
    enum NumberKind kind = number_kind(arguments[0]);
    long a;
    long b;

    if (kind == K_INTEGER && any_big(arguments[1], arguments[2])) {
        return make_bool(integer_compare(arguments[1], arguments[2]) < 0);
    }
    a = operand(kind, arguments[1]);
    b = operand(kind, arguments[2]);
    if (kind == K_WORD64) {
        return make_bool((unsigned long)a < (unsigned long)b);
    }
    return make_bool(a < b);
}

/* numAnd, numOr, numXor :: a -> a -> a */
static Value *bitwise(char operation, Value **arguments)
{
    enum NumberKind kind = number_kind(arguments[0]);
    unsigned long x;
    unsigned long y;

    if (kind == K_INTEGER && any_big(arguments[1], arguments[2])) {
        return integer_bitwise(operation, arguments[1], arguments[2]);
    }
    x = (unsigned long)operand(kind, arguments[1]);
    y = (unsigned long)operand(kind, arguments[2]);
    switch (operation) {
    case '&':
        return make_number(kind, x & y);
    case '|':
        return make_number(kind, x | y);
    default:
        return make_number(kind, x ^ y);
    }
}

Value *builtin_numAnd(Value **arguments)
{
    return bitwise('&', arguments);
}

Value *builtin_numOr(Value **arguments)
{
    return bitwise('|', arguments);
}

Value *builtin_numXor(Value **arguments)
{
    return bitwise('^', arguments);
}

/* numComplement :: a -> a */
Value *builtin_numComplement(Value **arguments)
{
    enum NumberKind kind = number_kind(arguments[0]);

    if (kind == K_INTEGER && integer_is_big(force(arguments[1]))) {
        /* complement n is -n - 1 */
        return integer_arithmetic('-', make_int(-1), arguments[1]);
    }
    return make_number(kind, ~(unsigned long)operand(kind, arguments[1]));
}

static int is_signed(enum NumberKind kind)
{
    return kind == K_INT || kind == K_INTEGER || kind == K_INT32
        || kind == K_INT16 || kind == K_INT8;
}

/* numShiftL, numShiftR :: a -> Int -> a
   Shifting right keeps the sign of a signed number. */
static Value *shift(int left, Value **arguments)
{
    enum NumberKind kind = number_kind(arguments[0]);
    long count = force(arguments[2])->number;
    unsigned long bits;
    long a;

    if (count < 0) {
        TODO("shift by a negative amount");
    }
    if (kind == K_INTEGER
        && (left || integer_is_big(force(arguments[1])))) {
        return integer_shift(arguments[1], count, left);
    }
    a = operand(kind, arguments[1]);
    bits = (unsigned long)a;
    if (left) {
        return make_number(kind, count >= 64 ? 0ul : bits << count);
    }
    if (count >= 64) {
        return make_number(kind, is_signed(kind) && a < 0 ? ~0ul : 0ul);
    }
    bits >>= count;
    if (is_signed(kind) && a < 0 && count > 0) {
        bits |= ~0ul << (64 - count); /* copies of the sign bit */
    }
    return make_number(kind, bits);
}

Value *builtin_numShiftL(Value **arguments)
{
    return shift(1, arguments);
}

Value *builtin_numShiftR(Value **arguments)
{
    return shift(0, arguments);
}

/* numConvert :: a -> b
   From one number type to another, as fromIntegral, toInteger, fromInteger,
   ord and chr do. */
Value *builtin_numConvert(Value **arguments)
{
    enum NumberKind from = number_kind(arguments[0]);
    enum NumberKind to = number_kind(arguments[1]);
    Value *value = force(arguments[2]);

    if (to == K_INTEGER) {
        if (from == K_WORD64 && value->number < 0) {
            /* A Word64 of 2^63 or more. */
            return integer_from_unsigned((unsigned long)value->number);
        }
        return from == K_INTEGER ? value : make_int(value->number);
    }
    if (from == K_INTEGER) {
        return make_number(to, integer_low_bits(value));
    }
    return make_number(to, (unsigned long)value->number);
}

/* numShow :: a -> String
   The digits, with a minus sign for a negative number. */
Value *builtin_numShow(Value **arguments)
{
    enum NumberKind kind = number_kind(arguments[0]);
    Value *value = force(arguments[1]);
    char text[32];

    if (kind == K_INTEGER) {
        return make_string(integer_show(value));
    }
    if (kind == K_WORD64) {
        sprintf(text, "%lu", (unsigned long)value->number);
    } else {
        sprintf(text, "%ld", value->number);
    }
    return make_string(text);
}
