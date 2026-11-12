#include "eval.h"

#include <string.h>

/* MD5, as RFC 1321 describes it. GHC.Fingerprint is MD5, and AIHC knows
   certain files by their fingerprint. */

typedef unsigned int Word; /* 32 bits: checked in numbers.c */

/* Each constant is the whole part of 2^32 * abs(sin(i)), i from 1. */
static const Word sines[64] = {
    0xd76aa478u, 0xe8c7b756u, 0x242070dbu, 0xc1bdceeeu, 0xf57c0fafu,
    0x4787c62au, 0xa8304613u, 0xfd469501u, 0x698098d8u, 0x8b44f7afu,
    0xffff5bb1u, 0x895cd7beu, 0x6b901122u, 0xfd987193u, 0xa679438eu,
    0x49b40821u, 0xf61e2562u, 0xc040b340u, 0x265e5a51u, 0xe9b6c7aau,
    0xd62f105du, 0x02441453u, 0xd8a1e681u, 0xe7d3fbc8u, 0x21e1cde6u,
    0xc33707d6u, 0xf4d50d87u, 0x455a14edu, 0xa9e3e905u, 0xfcefa3f8u,
    0x676f02d9u, 0x8d2a4c8au, 0xfffa3942u, 0x8771f681u, 0x6d9d6122u,
    0xfde5380cu, 0xa4beea44u, 0x4bdecfa9u, 0xf6bb4b60u, 0xbebfbc70u,
    0x289b7ec6u, 0xeaa127fau, 0xd4ef3085u, 0x04881d05u, 0xd9d4d039u,
    0xe6db99e5u, 0x1fa27cf8u, 0xc4ac5665u, 0xf4292244u, 0x432aff97u,
    0xab9423a7u, 0xfc93a039u, 0x655b59c3u, 0x8f0ccc92u, 0xffeff47du,
    0x85845dd1u, 0x6fa87e4fu, 0xfe2ce6e0u, 0xa3014314u, 0x4e0811a1u,
    0xf7537e82u, 0xbd3af235u, 0x2ad7d2bbu, 0xeb86d391u
};

/* How far each of the 64 steps rotates. */
static const int shifts[64] = {
    7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
    5, 9,  14, 20, 5, 9,  14, 20, 5, 9,  14, 20, 5, 9,  14, 20,
    4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
    6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21
};

static Word rotate_left(Word word, int count)
{
    return (word << count) | (word >> (32 - count));
}

/* Mixes one block of 64 bytes into the four words of the state. */
static void add_block(Word state[4], const unsigned char *block)
{
    Word m[16];
    Word a = state[0];
    Word b = state[1];
    Word c = state[2];
    Word d = state[3];
    int i;

    for (i = 0; i < 16; i++) {
        m[i] = (Word)block[4 * i] | ((Word)block[4 * i + 1] << 8)
            | ((Word)block[4 * i + 2] << 16) | ((Word)block[4 * i + 3] << 24);
    }
    for (i = 0; i < 64; i++) {
        Word f;
        Word next;
        int g;

        if (i < 16) {
            f = (b & c) | (~b & d);
            g = i;
        } else if (i < 32) {
            f = (d & b) | (~d & c);
            g = (5 * i + 1) % 16;
        } else if (i < 48) {
            f = b ^ c ^ d;
            g = (3 * i + 5) % 16;
        } else {
            f = c ^ (b | ~d);
            g = (7 * i) % 16;
        }
        next = b + rotate_left(a + f + sines[i] + m[g], shifts[i]);
        a = d;
        d = c;
        c = b;
        b = next;
    }
    state[0] += a;
    state[1] += b;
    state[2] += c;
    state[3] += d;
}

/* md5 :: ByteString -> ByteString
   The 16 bytes of the hash. */
Value *builtin_md5(Value **arguments)
{
    Value *input = force(arguments[0]);
    const unsigned char *bytes = input->bytes;
    unsigned long length = (unsigned long)input->number;
    Word state[4] = { 0x67452301u, 0xefcdab89u, 0x98badcfeu, 0x10325476u };
    unsigned char last[128];
    unsigned char *digest = xmalloc(16);
    unsigned long whole = length / 64 * 64;
    unsigned long rest = length - whole;
    unsigned long padded = rest < 56 ? 64 : 128;
    unsigned long bits = length * 8;
    unsigned long i;
    Value *result;

    if (input->kind != V_BYTES) {
        die("md5 was given something that is not a ByteString");
    }
    for (i = 0; i < whole; i += 64) {
        add_block(state, bytes + i);
    }
    /* The message ends with a 1 bit, zeros, and its length in bits as 64
       bits, lowest byte first, so that it fills whole blocks. */
    memset(last, 0, sizeof(last));
    if (rest > 0) {
        memcpy(last, bytes + whole, rest);
    }
    last[rest] = 0x80;
    for (i = 0; i < 8; i++) {
        last[padded - 8 + i] = (unsigned char)(bits >> (8 * i));
    }
    for (i = 0; i < padded; i += 64) {
        add_block(state, last + i);
    }
    for (i = 0; i < 16; i++) {
        digest[i] = (unsigned char)(state[i / 4] >> (8 * (i % 4)));
    }
    result = make_int(16);
    result->kind = V_BYTES;
    result->bytes = digest;
    return result;
}
