#include "eval.h"

#include <string.h>

/* SHA-256, as FIPS 180-4 describes it. AIHC names its cached files by the
   hash of what they were built from. */

typedef unsigned int Word; /* 32 bits: checked in numbers.c */

static const Word round_constants[64] = {
    0x428a2f98u, 0x71374491u, 0xb5c0fbcfu, 0xe9b5dba5u, 0x3956c25bu,
    0x59f111f1u, 0x923f82a4u, 0xab1c5ed5u, 0xd807aa98u, 0x12835b01u,
    0x243185beu, 0x550c7dc3u, 0x72be5d74u, 0x80deb1feu, 0x9bdc06a7u,
    0xc19bf174u, 0xe49b69c1u, 0xefbe4786u, 0x0fc19dc6u, 0x240ca1ccu,
    0x2de92c6fu, 0x4a7484aau, 0x5cb0a9dcu, 0x76f988dau, 0x983e5152u,
    0xa831c66du, 0xb00327c8u, 0xbf597fc7u, 0xc6e00bf3u, 0xd5a79147u,
    0x06ca6351u, 0x14292967u, 0x27b70a85u, 0x2e1b2138u, 0x4d2c6dfcu,
    0x53380d13u, 0x650a7354u, 0x766a0abbu, 0x81c2c92eu, 0x92722c85u,
    0xa2bfe8a1u, 0xa81a664bu, 0xc24b8b70u, 0xc76c51a3u, 0xd192e819u,
    0xd6990624u, 0xf40e3585u, 0x106aa070u, 0x19a4c116u, 0x1e376c08u,
    0x2748774cu, 0x34b0bcb5u, 0x391c0cb3u, 0x4ed8aa4au, 0x5b9cca4fu,
    0x682e6ff3u, 0x748f82eeu, 0x78a5636fu, 0x84c87814u, 0x8cc70208u,
    0x90befffau, 0xa4506cebu, 0xbef9a3f7u, 0xc67178f2u
};

static Word rotate_right(Word word, int count)
{
    return (word >> count) | (word << (32 - count));
}

/* Mixes one block of 64 bytes into the eight words of the state. */
static void add_block(Word state[8], const unsigned char *block)
{
    Word w[64];
    Word a = state[0];
    Word b = state[1];
    Word c = state[2];
    Word d = state[3];
    Word e = state[4];
    Word f = state[5];
    Word g = state[6];
    Word h = state[7];
    int i;

    for (i = 0; i < 16; i++) {
        w[i] = ((Word)block[4 * i] << 24) | ((Word)block[4 * i + 1] << 16)
            | ((Word)block[4 * i + 2] << 8) | (Word)block[4 * i + 3];
    }
    for (i = 16; i < 64; i++) {
        Word s0 = rotate_right(w[i - 15], 7) ^ rotate_right(w[i - 15], 18)
            ^ (w[i - 15] >> 3);
        Word s1 = rotate_right(w[i - 2], 17) ^ rotate_right(w[i - 2], 19)
            ^ (w[i - 2] >> 10);

        w[i] = w[i - 16] + s0 + w[i - 7] + s1;
    }
    for (i = 0; i < 64; i++) {
        Word s1 = rotate_right(e, 6) ^ rotate_right(e, 11) ^ rotate_right(e, 25);
        Word choice = (e & f) ^ (~e & g);
        Word t1 = h + s1 + choice + round_constants[i] + w[i];
        Word s0 = rotate_right(a, 2) ^ rotate_right(a, 13) ^ rotate_right(a, 22);
        Word majority = (a & b) ^ (a & c) ^ (b & c);
        Word t2 = s0 + majority;

        h = g;
        g = f;
        f = e;
        e = d + t1;
        d = c;
        c = b;
        b = a;
        a = t1 + t2;
    }
    state[0] += a;
    state[1] += b;
    state[2] += c;
    state[3] += d;
    state[4] += e;
    state[5] += f;
    state[6] += g;
    state[7] += h;
}

/* sha256 :: ByteString -> ByteString
   The 32 bytes of the hash. */
Value *builtin_sha256(Value **arguments)
{
    Value *input = force(arguments[0]);
    const unsigned char *bytes = input->bytes;
    unsigned long length = (unsigned long)input->number;
    Word state[8] = {
        0x6a09e667u, 0xbb67ae85u, 0x3c6ef372u, 0xa54ff53au,
        0x510e527fu, 0x9b05688cu, 0x1f83d9abu, 0x5be0cd19u
    };
    unsigned char last[128];
    unsigned char *digest = gc_alloc_bytes(32);
    unsigned long whole = length / 64 * 64;
    unsigned long rest = length - whole;
    unsigned long padded = rest < 56 ? 64 : 128;
    unsigned long bits = length * 8;
    unsigned long i;

    if (input->kind != V_BYTES) {
        die("sha256 was given something that is not a ByteString");
    }
    for (i = 0; i < whole; i += 64) {
        add_block(state, bytes + i);
    }
    /* The message ends with a 1 bit, zeros, and its length in bits as 64
       bits, so that it fills whole blocks. */
    memset(last, 0, sizeof(last));
    if (rest > 0) {
        memcpy(last, bytes + whole, rest);
    }
    last[rest] = 0x80;
    for (i = 0; i < 8; i++) {
        last[padded - 1 - i] = (unsigned char)(bits >> (8 * i));
    }
    for (i = 0; i < padded; i += 64) {
        add_block(state, last + i);
    }
    for (i = 0; i < 32; i++) {
        digest[i] = (unsigned char)(state[i / 4] >> (24 - 8 * (i % 4)));
    }
    return make_bytes(digest, 32);
}
