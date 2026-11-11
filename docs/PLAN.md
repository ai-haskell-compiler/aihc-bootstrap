# Plan for the library

The language side of the interpreter is mostly in place. What AIHC needs now
is mostly library: the sources that `scripts/run-aihc.sh` points at are 293
modules and about 163,000 lines, and they import 166 library modules.

This plan covers the large pieces, where one decision affects several others.
Small modules (`Data.Maybe`, `Data.Char`, ...) are written as the loop asks
for them.

## What AIHC uses

Measured over the first-party sources (uses, not modules):

| Library | Distinct functions | Most used |
| --- | ---: | --- |
| `Data.Text` | 65 | `unpack` 556, `pack` 322, `length`, `null`, `stripPrefix`, `intercalate` |
| `Data.Map` | 54 | `lookup` 729, `empty`, `fromList`, `insert`, `findWithDefault` |
| `Data.Set` | 24 | `member` 332, `fromList`, `empty`, `insert` |
| `Data.ByteString` | 41 | `length`, `pack`, `readFile`, `index`, `unpack`, `writeFile` |
| `Data.IntMap`, `Data.IntSet` | 23 | `empty`, `insert`, `member` |

Derived classes: `Show` 628, `Eq` 612, `Generic` 198, `Ord` 170, `NFData`
139, `Read` 133, `Data` 93, `Enum` 24, `Bounded` 21.

The first-party code defines only 3 classes and 228 instances. It has no type
families, no GADT-style declarations and no existential types of its own.
Those appear only in third-party packages, which is the main argument for
replacing those packages instead of running them.

## Rules for every replacement

1. **Write a small module in `lib/` with the upstream name.** It has only
   the functions AIHC uses.
2. **Keep the upstream algorithm where its output can be seen.** Examples:
   the order of `Data.Graph.stronglyConnComp`, the layout of
   `Prettyprinter`, the text of a derived `Show`. Anything else may be
   implemented in the simplest way.
3. **Test each module against the real one.** The test is a GHC-valid
   program, as now, run through AIHC's cabal project for third-party
   packages.

## Status

| Phase | State | Definitions prepared |
| --- | --- | ---: |
| 0 Survey mode | done | 1,766 of 10,399 |
| 1 Numbers and deriving | done, except as listed below | 2,808 of 10,845 |
| 2 Text, bytes and containers | done, except as listed below | 5,345 of 11,309 |
| 3 Monads and state | done | 8,009 of 11,566 |
| `Data.Graph`, `Data.Array`, `Data.Tree` | done | 8,523 of 11,610 |
| `Data.MonoidMap`, `Data.ByteString.Builder`, `Data.Binary.Get`/`Put` | done | 9,146 of 11,737 |
| 4 The system | next | |

Left over from phase 1, each stopping with a `TODO` when reached:

- `Integer` beyond a C `long` (values up to 2^64 - 1 can be held and
  converted, but not calculated with).
- `Double` and floating point literals.
- Derived `Read`, `Generic`, `Data`, `Functor`, `Foldable`, `Traversable`;
  `deriving via`.
- Data declarations with infix constructors.
- Character classification beyond ASCII (`Data.Char` is not written yet).

Left over from phase 2:

- `Data.Char` answers for ASCII only (and the no-break space); a question
  about another character stops with a `TODO`. `generalCategory` is not
  written.
- `Data.Text.Encoding.decodeUtf8With` on bytes that are not UTF-8.
- File handles other than stdout and stderr; `hGetContents`, `hGet`.
- `tests/monoidmap.hs` has no output from GHC to compare with, because GHC
  does not ship the package `monoidmap`. Its expected output was checked by
  hand.
- The unboxed and mutable arrays (`Data.Array.Unboxed`, `Data.Array.ST`),
  which the register allocator uses. `Data.Array` is a map from index to
  element, with `Int`-like indices only.
- `Data.Text`'s pattern synonyms (`:<`, `Empty`).

Done outside the phases, because the survey asked for them: export lists
(a module passes on the imported names it lists), data declarations with
infix constructors, and about twenty small modules of `base`
(`Control.Applicative`, `Data.Monoid`, `Data.Bits`, `Data.List.NonEmpty`,
...).

The larger blocks the survey still shows, by definitions stopped:
`Data.Text.Unsafe` 407, `Text.Megaparsec` 402, `Prettyprinter` 322,
`System.FilePath` 90, `GHC.Generics` 76, `System.Directory` 63,
`Data.Data` 62, `Control.Concurrent.STM` 58, `System.Process` 51.

## Phase 0: survey mode

Add a mode that loads every module AIHC can reach and elaborates every
definition without running anything, and that counts each `TODO` instead of
stopping at the first. The result is a ranked list of missing syntax and
missing library names. It replaces guessing in every phase below, and it is
the progress measure: definitions elaborated out of all definitions.

## Phase 1: numbers and deriving

Nothing else works without these.

- **Numbers.** `Int` and the `Word`/`Int` sizes are a C `long` with masking.
  `Integer` is a C `long` that stops with a `TODO` on overflow, so it is
  never silently wrong; a bignum is written when that `TODO` is reached.
  `Double` is a C `double`. The classes (`Num`, `Ord`, `Enum`, `Integral`,
  `Bits`, ...) are written in Haskell over one builtin per operation.
- **Deriving in C, not as generated code.** Types are at hand at run time
  and every value has the same shape (a tag and fields). So a derived
  instance is an instance whose methods are builtins: `==` compares tags and
  then calls `==` at the type of each field. One builtin per class covers
  every type. `Show` follows GHC's rules for precedence, records and infix
  constructors. `NFData` is a deep force. `Data` (`gmapT`, `gmapQ`,
  `gfoldl`) works the same way. `Read` and `Generic` wait for the survey:
  `read` and `reads` are used, but perhaps not at derived types.

## Phase 2: text, bytes and containers

- **`Text` is a newtype over `String`.** AIHC converts between the two
  constantly (878 calls to `pack` and `unpack`), and with a newtype those
  cost nothing. All 65 functions are list functions. If memory becomes the
  problem, the representation changes inside `lib/Data/Text.hs` only.
- **`ByteString` is a packed array in C.** `index` and `length` are used in
  ways that expect constant time, and the contents are whole object files.
  About 15 builtins; the rest is Haskell. A lazy `ByteString` is a newtype
  over a strict one.
- **`Map` and `Set` are one weight-balanced tree written in Haskell.** `Set
  a` is `Map a ()`. `IntMap` and `IntSet` are newtypes over them. The order
  of every result follows from the keys, so it matches upstream.
- **`Data.MonoidMap`, `Data.Graph`, `Data.Array`** are small modules over
  the above. `Data.Graph` is a port of the upstream algorithm (rule 2).

## Phase 3: monads and state

- **`transformers`** (`StateT`, `ReaderT`, `ExceptT`, `MaybeT`): about 40
  lines each, written directly.
- **`IORef`, `ST`, `STRef`:** one mutable cell builtin. `ST` is `IO`.
- **Exceptions:** `bracket`, `finally`, `evaluate`, `SomeException`
  hierarchy as far as AIHC catches things.

## Phase 4: the system

`System.FilePath` is pure Haskell. `System.Directory`, `System.Process`,
file I/O, clocks and `System.Environment` are builtins. AIHC uses 18
directory functions and runs external tools with `readProcessWithExitCode`
and `createProcess`.

This needs POSIX: ISO C89 has no directories, no `realpath` and no way to
capture the output of a process. See "Decisions".

## Phase 5: concurrency

AIHC uses `MVar`, `STM`, `async` and a worker pool sized by
`getNumCapabilities`. The plan is to stay single-threaded:

- `getNumCapabilities` is 1.
- `MVar` and `TVar` are mutable cells; `atomically` just runs.
- `async` does not start anything; `wait` runs the action. `withAsync` on a
  ticker that is never waited for therefore never runs it.

If AIHC's scheduler cannot work that way, the fallback is cooperative
threads inside the interpreter. The explicit stack makes that possible: a
thread is a stack, and a blocked `takeMVar` switches stacks. This is decided
after reading the scheduler, not before.

## Phase 6: parsing and printing

- **`megaparsec`** is the riskiest replacement. AIHC uses 17 modules of it,
  with a custom `Stream` instance, direct access to the parser state
  (`stateInput`, `stateOffset`, `updateParserState`), `observing`, and
  `errorBundlePretty`. A replacement is a real parser library of perhaps 600
  to 900 lines. It starts with a short study of `aihc-parser` to see how
  much of the state is really used.
- **`prettyprinter`** is a port of the upstream layout algorithm.

## Phase 7: data formats

`aeson` and `yaml` (lock files and plans), `binary`, and SHA-256 (in C; the
upstream package is C too). Sized after the survey shows which of them a
build actually reaches.

## Alongside: memory

Nothing is freed today, and a `Value` is over 100 bytes. A `String` costs a
few hundred bytes for each character. Compiling AIHC with AIHC will not fit
in memory like this. Two steps, in this order, as soon as a real compilation
is attempted:

1. **A garbage collector.** `main` runs inside `catchAny`, so the machine is
   always inside a nested call and C locals always hold values. The simplest
   collector that is correct here is mark and sweep that does not move
   values and scans the C stack conservatively. The alternative is to remove
   every nested call of the machine first.
2. **Smaller values.** One struct per kind instead of one flat struct.

## Language backlog

From the `LANGUAGE` pragmas in the sources, to be confirmed by the survey:
`PatternSynonyms` (21 modules), `ViewPatterns` (19), `LambdaCase` (19),
`BangPatterns` (15), `TypeApplications` (7), `TupleSections`, `MultiWayIf`,
`RankNTypes` (4). Also known to be missing: negation, arithmetic sequences,
list comprehensions, operators defined infix, export lists and re-exports,
multi-parameter classes (one use, `MonadState`).

## Decisions

Made:

- **POSIX is allowed.** The interpreter stays C89 as a language and may call
  POSIX functions (`opendir`, `mkdir`, `realpath`, `fork`, `execvp`, `pipe`,
  `waitpid`). They are kept in one file, apart from the rest.

- **Printed messages may differ; compiled output may not.** The binary that
  an interpreted AIHC builds must, when it compiles AIHC again, produce a
  byte-for-byte identical result. So help text, error messages and
  pretty-printed dumps are free, but everything that can reach generated
  code must behave exactly as with GHC: the order of every traversal
  (`Data.Graph`, `nub`, sorting and its stability), hashes, number
  formatting, and any `Show` output that ends up in a symbol name or a
  file. Rule 2 above is read this way.

- **Deriving is done in C** (`src/derive.c`), not as generated instances.
