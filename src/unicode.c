#include "eval.h"

/* What Data.Char knows about a character: its general category, its upper,
   lower and title case, and whether it is upper or lower case. The answers
   are GHC's, copied into a table by scripts/gen-unicode.hs, so that the
   lexer of the compiler we run sees every character as GHC does. */

typedef struct Run {
    long start; /* the first character of the run */
    long value;
} Run;

#include "unicode_table.h"

/* The value of the run the character is in. */
static long look_up(const Run *runs, long count, long character)
{
    long low = 0;
    long high = count - 1;

    while (low < high) {
        long middle = (low + high + 1) / 2;

        if (runs[middle].start <= character) {
            low = middle;
        } else {
            high = middle - 1;
        }
    }
    return runs[low].value;
}

#define LOOK_UP(runs, character) \
    look_up(runs, (long)(sizeof(runs) / sizeof(runs[0])), character)

/* charInfo :: Int -> Char -> Int
   0: the general category, as the position of the constructor in
      GeneralCategory
   1, 2, 3: the character in upper, lower and title case
   4, 5: 1 if it is upper case, lower case */
Value *builtin_charInfo(Value **arguments)
{
    long what = force(arguments[0])->number;
    long character = force(arguments[1])->number;

    switch (what) {
    case 0:
        return make_int(LOOK_UP(category_runs, character));
    case 1:
        return make_int(character + LOOK_UP(upper_runs, character));
    case 2:
        return make_int(character + LOOK_UP(lower_runs, character));
    case 3:
        return make_int(character + LOOK_UP(title_runs, character));
    case 4:
        return make_int(LOOK_UP(is_upper_runs, character));
    default:
        return make_int(LOOK_UP(is_lower_runs, character));
    }
}
