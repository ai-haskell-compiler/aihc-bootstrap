#include "eval.h"

#include <limits.h>
#include <stdio.h>
#include <string.h>

/* Integer. A value that fits in a C long is that long, as for every other
   number type. A larger one is a sign and a magnitude:

       number        the sign, 1 or -1
       tag           INTEGER_BIG
       field_count   how many limbs the magnitude has
       bytes         the limbs, least significant first

   A limb is 32 bits, so that two of them can be multiplied in an unsigned
   long. The result of every operation is made small again if it fits, so
   a value has exactly one form and the rest of the interpreter never sees
   a big value that could be a long.

   The algorithms are the ones done by hand: readability first. */

typedef unsigned int Limb;
typedef char limb_must_be_32_bits[sizeof(Limb) == 4 ? 1 : -1];

#define LIMB_BITS 32
#define LIMB_BASE 0x100000000ul
#define LIMB_MASK 0xFFFFFFFFul

/* A number while it is being worked on. */
typedef struct Big {
    int sign;    /* 1 or -1; a zero has sign 1 */
    int count;   /* limbs in use: the last one is not zero */
    Limb *limbs;
} Big;

static Big new_big(int count)
{
    Big big;
    int i;

    big.sign = 1;
    big.count = count;
    big.limbs = gc_alloc_bytes((size_t)count * sizeof(Limb) + 1);
    for (i = 0; i < count; i++) {
        big.limbs[i] = 0;
    }
    return big;
}

static void trim(Big *big)
{
    while (big->count > 0 && big->limbs[big->count - 1] == 0) {
        big->count--;
    }
    if (big->count == 0) {
        big->sign = 1;
    }
}

static Big big_from_magnitude(unsigned long magnitude, int sign)
{
    Big big = new_big(2);

    big.limbs[0] = (Limb)(magnitude & LIMB_MASK);
    big.limbs[1] = (Limb)(magnitude >> LIMB_BITS);
    big.sign = sign;
    trim(&big);
    return big;
}

int integer_is_big(Value *value)
{
    return value->tag == INTEGER_BIG;
}

static Big big_of(Value *value)
{
    Big big;

    value = force(value);
    if (!integer_is_big(value)) {
        long number = value->number;

        return number < 0 ? big_from_magnitude(0ul - (unsigned long)number, -1)
                          : big_from_magnitude((unsigned long)number, 1);
    }
    big.sign = (int)value->number;
    big.count = value->field_count;
    big.limbs = (Limb *)value->bytes;
    return big;
}

/* The value of the number: a long if it fits. */
static Value *value_of(Big big)
{
    Value *value;

    trim(&big);
    if (big.count <= 2) {
        unsigned long magnitude = big.count > 0 ? big.limbs[0] : 0;

        if (big.count == 2) {
            magnitude |= (unsigned long)big.limbs[1] << LIMB_BITS;
        }
        if (big.sign > 0 && magnitude <= (unsigned long)LONG_MAX) {
            return make_int((long)magnitude);
        }
        if (big.sign < 0 && magnitude <= (unsigned long)LONG_MAX + 1) {
            return make_int((long)(0ul - magnitude));
        }
    }
    /* A number with all the fields: the sign, how many limbs, the limbs. */
    value = make_bytes((const unsigned char *)big.limbs, big.sign);
    value->kind = V_INT;
    value->tag = INTEGER_BIG;
    value->field_count = big.count;
    return value;
}

Value *integer_from_unsigned(unsigned long number)
{
    return value_of(big_from_magnitude(number, 1));
}

/* ---- Magnitudes ---- */

/* -1, 0 or 1 */
static int compare_magnitudes(Big a, Big b)
{
    int i;

    if (a.count != b.count) {
        return a.count < b.count ? -1 : 1;
    }
    for (i = a.count - 1; i >= 0; i--) {
        if (a.limbs[i] != b.limbs[i]) {
            return a.limbs[i] < b.limbs[i] ? -1 : 1;
        }
    }
    return 0;
}

static Big add_magnitudes(Big a, Big b)
{
    Big sum = new_big((a.count > b.count ? a.count : b.count) + 1);
    unsigned long carry = 0;
    int i;

    for (i = 0; i < sum.count; i++) {
        unsigned long digit = carry;

        if (i < a.count) {
            digit += a.limbs[i];
        }
        if (i < b.count) {
            digit += b.limbs[i];
        }
        sum.limbs[i] = (Limb)(digit & LIMB_MASK);
        carry = digit >> LIMB_BITS;
    }
    trim(&sum);
    return sum;
}

/* a - b, where a is not smaller than b. */
static Big subtract_magnitudes(Big a, Big b)
{
    Big difference = new_big(a.count);
    unsigned long borrow = 0;
    int i;

    for (i = 0; i < a.count; i++) {
        unsigned long take = borrow + (i < b.count ? b.limbs[i] : 0);
        unsigned long digit = a.limbs[i];

        if (digit < take) {
            digit += LIMB_BASE;
            borrow = 1;
        } else {
            borrow = 0;
        }
        difference.limbs[i] = (Limb)(digit - take);
    }
    trim(&difference);
    return difference;
}

static Big multiply_magnitudes(Big a, Big b)
{
    Big product = new_big(a.count + b.count);
    int i;
    int j;

    for (i = 0; i < a.count; i++) {
        unsigned long carry = 0;

        for (j = 0; j < b.count; j++) {
            unsigned long digit = (unsigned long)a.limbs[i] * b.limbs[j]
                + product.limbs[i + j] + carry;

            product.limbs[i + j] = (Limb)(digit & LIMB_MASK);
            carry = digit >> LIMB_BITS;
        }
        product.limbs[i + b.count] = (Limb)carry;
    }
    trim(&product);
    return product;
}

static int bit_length(Big a)
{
    Limb top;
    int bits;

    if (a.count == 0) {
        return 0;
    }
    top = a.limbs[a.count - 1];
    bits = (a.count - 1) * LIMB_BITS;
    while (top != 0) {
        bits++;
        top >>= 1;
    }
    return bits;
}

static int bit_at(Big a, int index)
{
    int limb = index / LIMB_BITS;

    return limb < a.count && ((a.limbs[limb] >> (index % LIMB_BITS)) & 1u);
}

static Big shift_magnitude_left(Big a, long count)
{
    int limbs = (int)(count / LIMB_BITS);
    int bits = (int)(count % LIMB_BITS);
    Big result = new_big(a.count + limbs + 1);
    int i;

    for (i = 0; i < a.count; i++) {
        unsigned long digit = (unsigned long)a.limbs[i] << bits;

        result.limbs[i + limbs] |= (Limb)(digit & LIMB_MASK);
        result.limbs[i + limbs + 1] |= (Limb)(digit >> LIMB_BITS);
    }
    trim(&result);
    return result;
}

/* Sets *lost if a bit that was 1 is shifted out. */
static Big shift_magnitude_right(Big a, long count, int *lost)
{
    int limbs = (int)(count / LIMB_BITS);
    int bits = (int)(count % LIMB_BITS);
    Big result = new_big(a.count > limbs ? a.count - limbs : 0);
    int i;

    *lost = 0;
    for (i = 0; i < a.count && i < limbs; i++) {
        *lost = *lost || a.limbs[i] != 0;
    }
    if (limbs < a.count && bits > 0
        && (a.limbs[limbs] & ((1u << bits) - 1u)) != 0) {
        *lost = 1;
    }
    for (i = 0; i < result.count; i++) {
        unsigned long digit = a.limbs[i + limbs];

        if (i + limbs + 1 < a.count) {
            digit |= (unsigned long)a.limbs[i + limbs + 1] << LIMB_BITS;
        }
        result.limbs[i] = (Limb)((digit >> bits) & LIMB_MASK);
    }
    trim(&result);
    return result;
}

/* Long division, one bit at a time: the quotient and the remainder of the
   magnitudes. */
static Big divide_magnitudes(Big a, Big b, Big *remainder)
{
    Big quotient = new_big(a.count);
    Big rest = new_big(b.count + 1);
    int i;

    rest.count = 0;
    for (i = bit_length(a) - 1; i >= 0; i--) {
        /* rest = rest * 2 + the next bit of a */
        unsigned long carry = (unsigned long)bit_at(a, i);
        int k;

        for (k = 0; k < rest.count; k++) {
            unsigned long digit = ((unsigned long)rest.limbs[k] << 1) | carry;

            rest.limbs[k] = (Limb)(digit & LIMB_MASK);
            carry = digit >> LIMB_BITS;
        }
        if (carry != 0) {
            rest.limbs[rest.count++] = (Limb)carry;
        }
        if (compare_magnitudes(rest, b) >= 0) {
            Big smaller = subtract_magnitudes(rest, b);

            memcpy(rest.limbs, smaller.limbs,
                   (size_t)smaller.count * sizeof(Limb));
            rest.count = smaller.count;
            quotient.limbs[i / LIMB_BITS] |= 1u << (i % LIMB_BITS);
        }
    }
    trim(&quotient);
    *remainder = rest;
    return quotient;
}

/* ---- Numbers with a sign ---- */

static Big add(Big a, Big b)
{
    Big result;

    if (a.sign == b.sign) {
        result = add_magnitudes(a, b);
        result.sign = a.sign;
    } else if (compare_magnitudes(a, b) >= 0) {
        result = subtract_magnitudes(a, b);
        result.sign = a.sign;
    } else {
        result = subtract_magnitudes(b, a);
        result.sign = b.sign;
    }
    trim(&result);
    return result;
}

static Big negated(Big a)
{
    a.sign = a.count == 0 ? 1 : -a.sign;
    return a;
}

Value *integer_arithmetic(char operation, Value *left, Value *right)
{
    Big a = big_of(left);
    Big b = big_of(right);
    Big result;

    switch (operation) {
    case '+':
        return value_of(add(a, b));
    case '-':
        return value_of(add(a, negated(b)));
    default:
        result = multiply_magnitudes(a, b);
        result.sign = a.sign * b.sign;
        return value_of(result);
    }
}

/* quot and rem round towards zero ('q', 'r'); div and mod round down
   ('d', 'm'). The divisor is not zero. */
Value *integer_division(char operation, Value *left, Value *right)
{
    Big a = big_of(left);
    Big b = big_of(right);
    Big remainder;
    Big quotient = divide_magnitudes(a, b, &remainder);

    quotient.sign = a.sign * b.sign;
    remainder.sign = a.sign;
    trim(&quotient);
    trim(&remainder);
    if ((operation == 'd' || operation == 'm') && remainder.count > 0
        && remainder.sign != b.sign) {
        quotient = add(quotient, big_from_magnitude(1, -1));
        remainder = add(remainder, b);
    }
    return value_of(operation == 'q' || operation == 'd' ? quotient
                                                         : remainder);
}

int integer_compare(Value *left, Value *right)
{
    Big a = big_of(left);
    Big b = big_of(right);

    if (a.sign != b.sign) {
        return a.sign < b.sign ? -1 : 1;
    }
    return a.sign * compare_magnitudes(a, b);
}

/* Shifting right rounds down, as for every signed number. */
Value *integer_shift(Value *value, long count, int left)
{
    Big a = big_of(value);
    Big result;
    int lost;

    if (left) {
        result = shift_magnitude_left(a, count);
        result.sign = a.sign;
        return value_of(result);
    }
    result = shift_magnitude_right(a, count, &lost);
    result.sign = a.sign;
    trim(&result);
    if (a.sign < 0 && lost) {
        result = add(result, big_from_magnitude(1, -1));
    }
    return value_of(result);
}

/* The number as `count` limbs in two's complement. */
static Limb *twos_complement(Big a, int count)
{
    Limb *limbs = gc_alloc_bytes((size_t)count * sizeof(Limb) + 1);
    unsigned long carry = 1;
    int i;

    for (i = 0; i < count; i++) {
        limbs[i] = i < a.count ? a.limbs[i] : 0;
    }
    if (a.sign < 0) {
        for (i = 0; i < count; i++) {
            unsigned long digit = (unsigned long)(~limbs[i] & LIMB_MASK) + carry;

            limbs[i] = (Limb)(digit & LIMB_MASK);
            carry = digit >> LIMB_BITS;
        }
    }
    return limbs;
}

/* and, or, xor ('&', '|', '^'), as if both numbers were written in two's
   complement with as many bits as it takes. */
Value *integer_bitwise(char operation, Value *left, Value *right)
{
    Big a = big_of(left);
    Big b = big_of(right);
    int count = (a.count > b.count ? a.count : b.count) + 1;
    Limb *x = twos_complement(a, count);
    Limb *y = twos_complement(b, count);
    Big result = new_big(count);
    int negative;
    int i;

    for (i = 0; i < count; i++) {
        result.limbs[i] = operation == '&' ? (x[i] & y[i])
            : operation == '|' ? (x[i] | y[i]) : (x[i] ^ y[i]);
    }
    /* The top bit is the sign. */
    negative = (result.limbs[count - 1] >> (LIMB_BITS - 1)) & 1u;
    if (negative) {
        Limb *magnitude;

        result.sign = -1; /* so that twos_complement negates it */
        magnitude = twos_complement(result, count);
        result.limbs = magnitude;
    }
    trim(&result);
    if (negative) {
        result.sign = -1;
    }
    return value_of(result);
}

/* The lowest 64 bits of the number in two's complement: what it is as an
   Int or a Word64. */
unsigned long integer_low_bits(Value *value)
{
    Big a = big_of(value);
    Limb *limbs = twos_complement(a, 2);

    return (unsigned long)limbs[0] | ((unsigned long)limbs[1] << LIMB_BITS);
}

/* ---- Text ---- */

/* The number in decimal. */
char *integer_show(Value *value)
{
    Big a = big_of(value);
    /* Ten digits are more than enough for each limb, plus the sign. */
    char *text = gc_alloc_bytes((size_t)a.count * 10 + 3);
    char *end = text + a.count * 10 + 2;
    char *p = end;
    Big ten = big_from_magnitude(1000000000ul, 1);
    int negative = a.sign < 0;

    *p = '\0';
    if (a.count == 0) {
        *--p = '0';
    }
    a.sign = 1;
    while (a.count > 0) {
        Big remainder;
        unsigned long digits;
        int i;

        a = divide_magnitudes(a, ten, &remainder);
        digits = remainder.count > 0 ? remainder.limbs[0] : 0;
        /* Nine digits at a time; the leading zeros of the last group are
           left out. */
        for (i = 0; i < 9 && (a.count > 0 || digits > 0); i++) {
            *--p = (char)('0' + digits % 10);
            digits /= 10;
        }
    }
    if (negative) {
        *--p = '-';
    }
    return p;
}

/* The number that the literal is: decimal, or with 0x, 0o or 0b in front.
   Underscores between the digits are skipped. */
Value *integer_from_literal(const char *text)
{
    unsigned long base = 10;
    Big result = new_big(0);

    if (text[0] == '0' && text[1] != '\0' && strchr("xXoObB", text[1])) {
        base = (text[1] | 0x20) == 'x' ? 16 : (text[1] | 0x20) == 'o' ? 8 : 2;
        text += 2;
    }
    for (; *text != '\0'; text++) {
        unsigned long digit;

        if (*text == '_') {
            continue;
        }
        digit = *text <= '9' ? (unsigned long)(*text - '0')
                             : (unsigned long)((*text | 0x20) - 'a' + 10);
        result = add(multiply_magnitudes(result, big_from_magnitude(base, 1)),
                     big_from_magnitude(digit, 1));
    }
    return value_of(result);
}

/* The number as sign * top * 2^shift, where `top` has its highest bits and
   the bits below them are dropped. If any dropped bit was 1, the lowest
   bit of `top` is set: then rounding `top` to a floating point number
   rounds the whole number correctly. */
void integer_top_bits(Value *value, unsigned long *top, long *shift,
                      int *negative)
{
    Big a = big_of(value);
    int length = bit_length(a);
    int lost = 0;
    Big high;

    *negative = a.sign < 0;
    *shift = length > 64 ? length - 64 : 0;
    high = shift_magnitude_right(a, *shift, &lost);
    *top = high.count > 0 ? high.limbs[0] : 0;
    if (high.count > 1) {
        *top |= (unsigned long)high.limbs[1] << LIMB_BITS;
    }
    if (lost) {
        *top |= 1ul;
    }
}
