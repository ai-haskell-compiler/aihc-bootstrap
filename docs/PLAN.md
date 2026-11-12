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
| `Data.Text.Unsafe` | done | |
| 4 The system | done, except as listed below | 9,459 of 11,833 |
| 5 Concurrency | done | 9,605 of 11,901 |
| 6 Parsing: megaparsec | done | 10,625 of 12,140 |
| 6 Printing: prettyprinter | done | 11,056 of 12,263 |
| Language backlog, first part | done | 11,685 of 12,292 |
| Type applications; arrays and vectors | done | 12,025 of 12,404 |
| `forall` in types, `Data`, `Read`, `Numeric` | done | 12,273 of 12,499 |
| `Integer` of any size, `Rational`, `Double`, `Float` | done | 12,965 of 13,143 |
| Handles, Unicode, SHA-256, times | done | 13,125 of 13,211 |
| 7 Data formats: JSON, MD5 | done | |
| Running AIHC | in progress: see below | |

Left over from phase 1, each stopping with a `TODO` when reached:

- Division by zero as an exception.
- Derived `Read`, `Functor`, `Foldable`, `Traversable`; `deriving via`.

Numbers, as they are now:

- **`Integer`** is a C `long` while it fits and a sign with 32-bit limbs
  otherwise (`src/integer.c`), with the algorithms done by hand. Every
  result is made small again if it fits, so a number has one form.
  `tests/integer.hs` runs about 4,000 pairs of numbers around each size
  boundary through every operation.
- **`Double` and `Float`** are their IEEE bits. Arithmetic is C's. The two
  things C does not promise are done in Haskell with `Integer`
  (`lib/GHC/Float.hs`), following GHC's base: a fraction to the nearest
  number, rounded once (literals, `read`, `fromRational`), and the
  shortest digits that mean the same number (`show`). `tests/floating.hs`
  compares the bits of literals and the text of `show` with GHC.
- **`Rational`** is `Data.Ratio`, as in the Haskell report.
- **A literal like `1.5`** is `fromRational` of its exact fraction. A type
  that nothing decides and that is used as a fraction is `Double`.

Left over from phase 4:

- `createProcess`, `waitForProcess` and `terminateProcess`: a program that
  runs alongside the interpreter. Programs that are run and waited for
  (`readProcessWithExitCode` and its relatives) work.
- Running a program with another environment (`env`).
- `timeout`.
- File locks (`GHC.IO.Handle.Lock`) do nothing: two runs at the same time
  must not share a build directory.

Done since: file handles (`tests/handles.hs`), modification times and the
environment (`tests/time.hs`), SHA-256 in C (`tests/sha256.hs`, checked
against `shasum`), and Unicode: what `Data.Char` says about a character
comes from a table made with GHC by `scripts/gen-unicode.hs`
(`tests/unicode.hs`).

The interpreter runs on a stack of 4 GB of address space (`src/posix.c`),
because evaluating a value inside a value is a C call inside a C call.


Left over from phase 2:

- `Data.Char` answers for ASCII only (and the no-break space); a question
  about another character stops with a `TODO`. `generalCategory` is not
  written.
- `Data.Text.Encoding.decodeUtf8With` on bytes that are not UTF-8.
- File handles other than stdout and stderr; `hGetContents`, `hGet`.
- `tests/monoidmap.hs` and `tests/async.hs` have no output from GHC to
  compare with, because GHC does not ship the packages `monoidmap` and
  `async`. Their expected output was checked by hand.
- `Data.Array` only takes indices that can be enumerated (AIHC uses
  `Int`), where upstream has the class `Ix`.
- `Data.Text`'s pattern synonyms (`:<`, `Empty`).

Done outside the phases, because the survey asked for them: export lists
(a module passes on the imported names it lists), data declarations with
infix constructors, and about twenty small modules of `base`
(`Control.Applicative`, `Data.Monoid`, `Data.Bits`, `Data.List.NonEmpty`,
...).

What the survey still shows, by definitions stopped: `Data.Aeson` 29, a
type mismatch in `Aihc.Native.Emit` 18, `eqT` 6, `GHC.Fingerprint` (MD5)
6, and about twenty kinds of failure that stop five definitions or fewer.

Arrays: `Data.Array`, `Data.Array.ST`, the vectors (`Data.Vector` and its
mutable and unboxed variants) and the primitive arrays are all one array
of the interpreter's (`lib/Interpreter/Array.hs`, six builtins), each with
its own interface. Nothing is unboxed. Mutable byte arrays are a
`ByteString` that three builtins overwrite. Only `ST` is supported as the
monad, and `tests/vectors.hs` was checked by hand.

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

- **`Text` is a `ByteString` that is UTF-8**, as upstream. It was a
  newtype over `String` first; the lexer of `aihc-parser` works with byte
  offsets and the length in bytes at every token, which that made far too
  slow. A part of a `Text` shares the bytes of the whole. What only looks
  at a text works on the bytes; what makes a new text anyway (`map`,
  `filter`, `reverse`) goes through the list of characters. Three
  primitives are in C: the character at an offset, the number of
  characters, the offset after n characters.
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
`getNumCapabilities`. The interpreter stays single-threaded:

- `getNumCapabilities` is 1.
- `MVar`, `TVar` and `TMVar` are mutable cells; `atomically` just runs.
- An operation that would wait for another thread (`takeMVar` on an empty
  `MVar`, `retry`) throws the exception GHC throws for a thread that can
  never continue. AIHC's task graph only retries when no task is ready
  and some remain, which one worker never sees: a task is only added with
  dependencies that are in the graph.
- `async` does not start anything; the first `wait` runs the action.
  `withAsync` on a ticker that is never waited for therefore never runs
  it.
- A transaction that ends with an exception keeps what it wrote.

If some part of AIHC cannot work that way, the fallback is cooperative
threads inside the interpreter. The explicit stack makes that possible: a
thread is a stack, and a blocked `takeMVar` switches stacks.

## Phase 6: parsing and printing

- **`megaparsec`** is a port of the upstream core (`lib/Text/Megaparsec/`),
  definition by definition, because which error a parser reports decides
  what the parsers built on it do. `tests/megaparsec.hs` compares about
  900 lines of results and errors with upstream, run from its source.
  Three things differ:
  - The parser's continuations return an opaque `Answer` where upstream
    has `forall b. m b`, and nothing runs under a parser: `ParsecT e s m`
    ignores its `m`.
  - Upstream's class `MonadParsec e s m` has three parameters. Here it has
    `m` only, with `e` and `s` as associated types. That needed
    associated types in the interpreter, which megaparsec's `Stream` class
    needs anyway (`tests/families.hs`).
  - `Text.Megaparsec.Char.Lexer.float` waits for `Double`.
- **`prettyprinter`** is a port of the upstream document type and layout
  algorithm (`lib/Prettyprinter/Internal.hs`). AIHC writes files with it,
  so the same document has to give the same text. `tests/prettyprinter.hs`
  compares about 1,250 lines with upstream, run from its source, at six
  page widths. One thing differs: the ribbon fraction of a page width is
  an `Int`, because there is no `Double` yet. AIHC only uses 1.

## Phase 7: data formats

`aeson` and `yaml` (lock files and plans), `binary`, and SHA-256 (in C; the
upstream package is C too). Sized after the survey shows which of them a
build actually reaches.

## Running AIHC

`scripts/run-aihc.sh` now runs AIHC itself: the AIHC of the submodule
`vendor/aihc`, with its three libraries beside it, each pinned to a
commit. `aihc build /path/to/Main.hs` reads its options, finds the core libraries, solves
the build plan and prints it, and starts to compile:

```text
  aihc-prim-0.13.0
  aihc-base-4.21.2.0
  Main (executable)
build  aihc-rts-1.0.2 (no modules)
```

The runtime system is the first package. AIHC gives its six C sources to
the C compiler, compiles `rts.lir` to an object file itself, and puts them
in an archive. **Every file of the package is byte for byte what AIHC
built with GHC writes**: the object file AIHC generates, the archive, and
the manifest with the package's hash.

```text
built  aihc-rts-1.0.2 in 1 min 19 s
aihc-rts: 11 identical, 0 different, 0 missing
```

AIHC built with GHC takes 0.6 seconds for that package, so the
interpreter is about 130 times slower. Then come `aihc-prim` (27 modules)
and `aihc-base` (280 modules), which AIHC built with GHC compiles in 4
and 5 seconds.

Both libraries, 2 MB of source, are lexed and parsed in about 20 minutes.
Then AIHC resolves the names of `aihc-prim`, which is where it is now.
Three things were found on the way, each by looking at where the
collections find the program (`AIHC_GC_REPORT=1`):

- **The lexer took time with the square of a file's length.** Before every
  token it asks for the length of the rest of the input in bytes
  (`lengthWord8`) and reads characters at byte offsets (`iter`). A `Text`
  was a list of characters, so both walked the list. A `Text` is now its
  characters in UTF-8, and both are one step.
- **`T.unpack` of the rest of a file**, of which only the start is looked
  at (`findIndex` in the scanner for string literals). A long `Text` is
  now unpacked as far as the list is looked at.
- **A union of maps took time with the left map**, which is the large one
  when a program adds a little to what it has: `acc <> new`. A union now
  puts the smaller map into the larger one (`Data.Map`, `Data.MonoidMap`).

### Comparing a build with the real one

Build the same program with both into two stores, and `cmp` the files.
Three things have to be the same for the files to be:

- **The sources of AIHC.** The interpreter runs the sources in `vendor/`.
  The AIHC to compare with has to be built from the same commits:
  `cabal build aihc` in `vendor/aihc`, which takes the three libraries at
  the versions that `vendor/` pins (`aihc-parser` 5.0.0.0, `aihc-cpp`
  2.0.0.0, `aihc-cabal-syntax` 2.0.0.0). Point it at the data files of the
  submodule with `aihc_datadir`.
- **The compiler's identity.** AIHC puts a hash of its own executable into
  the hash of every package. Set `AIHC_COMPILER_IDENTITY` to the same
  value for both.
- **The directory.** AIHC reads the core libraries from `core-libs/` of
  the directory it runs in. `scripts/run-aihc.sh` runs it in
  `build/aihc-work`, where `core-libs` is a link; run the other one there
  too.

```sh
AIHC_COMPILER_IDENTITY=x sh scripts/run-aihc.sh build /path/Main.hs --build-root /tmp/a/target --store /tmp/a/store
cd build/aihc-work
AIHC_COMPILER_IDENTITY=x aihc_datadir=../../vendor/aihc/bin/aihc aihc build /path/Main.hs --build-root /tmp/b/target --store /tmp/b/store
```

To move the pin, check out another commit in `vendor/aihc`, and in each
library the commit of the version that `vendor/aihc/cabal.project` names.

### Where the time goes

`sample` on the running interpreter shows it. Three things were most of
the time of a build, and each took a few lines to fix:

| What | Share | Now |
| --- | ---: | --- |
| Looking through all instances of all classes for every method call | 60% | Each class has a list of its instances (`collect_instances`) |
| Finding the block of a cell that is being allocated | 16% | The block of the last cell of that size is tried first |
| Finding the block of a word while marking | 40% of what was left | The search looks only at the bases of the blocks, in one array |

## Memory

`src/gc.c` frees what the program no longer refers to. The machine is
always inside nested C calls (`force` inside a builtin inside `run`), and
C locals hold values, so the collector does not move anything and reads
the C stack as it is: every word that is the address of something
allocated keeps that alive. It has an allocator of its own, in cells of a
few sizes, so that a word can be checked against what exists.

`make test-gc` runs the tests with a collection after every 100 kB.
`AIHC_GC_REPORT=1` prints a line for every collection, and the lines of
the program that the machine is at. A collection comes at no particular
place, so many of them show where a program spends its time.

Three things kept memory in use that a compiled program would have let go.
All three had to be fixed before AIHC ran in constant memory, and
`tests/memory.hs` runs into each of them:

1. **A thunk or a closure kept everything that was in scope** where it was
   made. It now keeps only the variables its expression mentions
   (`keep_needed` in `src/eval.c`).
2. **A thunk in tail position left a frame behind**: ``a `seq` b`` gives
   back `b`, which is a thunk, so a strict loop grew the stack by a frame
   for every turn. Such a thunk now shares the frame of the thunk that is
   waiting for it.
3. **A function that calls itself passed its type on as a thunk**, which
   referred to the thunk of the call before. A type that is a type
   variable is now passed as it is.

Types made while the program runs are freed too. A type is made
whenever a type with variables is passed to a function, and those types
are only referred to by values. They are allocated by the collector; the
types of the program itself (signatures, what inference finds) are not,
and must never refer to one that is (`new_type` in `src/types.c`).

What is left:

- **Smaller values.** A `Value` is 128 bytes whatever it is, and a thunk
  also takes a cell of 32 bytes for each variable it keeps. One struct
  per kind, or an allocation that is only as long as the kind needs.
  While AIHC parses the two libraries the process grows to about 4 GB.

The megaparsec port takes more than linear time: 4,000 words parse in 2.5
seconds and 16,000 in 11 (measured before the three fixes under "Where
the time goes"). AIHC's own inputs are small enough for that not to
matter yet.

## Language backlog

Done, each with a test compared against GHC:

| Feature | Test |
| --- | --- |
| Associated types of classes | `tests/families.hs` |
| `\case`, view patterns, tuple sections, named field puns, `let` in guards (with functions and several definitions), bang patterns in clauses, operators defined between their parameters, qualified constructor operators, a pattern binding with a type and guards, `~` in front of a let pattern, `(pattern :: type)`, a `do` block that ends at an operator, a local function made without naming its parameter and used at two types | `tests/extensions.hs` |
| Local functions that call each other, without signatures | `tests/recursion.hs` |
| Pattern synonyms: both directions, with a `where` clause, with fields (also to build, to update and in a `let`), as operators | `tests/patsyn.hs` |
| Export lists that hide a module's own definitions; pattern synonyms exported as part of a type | `tests/exports.hs` |
| Type applications (`f @T`), with the type variables of a signature in scope in its body; type variables of their own in expression annotations | `tests/typeapp.hs` |
| A pattern that is a variable or `_` does not evaluate what it is matched with | `tests/laziness.hs` |
| `forall` inside a type: an argument that works at every type, and a record field that does | `tests/rankn.hs` |
| Derived `Data` (`gmapT`, `gmapQ`, `gmapM`); `eqT`, and a match on `Refl` | `tests/data.hs` |
| A pattern in a `do` block that does not match is the monad's `fail` | `tests/dofail.hs` |
| Derived `Functor`, `Foldable` and `Traversable` | `tests/derivefunctor.hs` |
| `deriving (Semigroup, Monoid) via Generically T` | `tests/derivingvia.hs` |

How the less obvious ones work:

- **A view pattern** `(f -> p)` matches a value `v` if `p` matches `f v`.
- **Local functions that call each other** are inferred as a group and
  generalised over one list of type variables (`infer_binding`).
- **A pattern synonym** used in a pattern is replaced by its right-hand
  side, read again from the tokens for each use and resolved in the module
  of the synonym, with the arguments of the use in place of the
  parameters. Used in an expression it is a function. A parameter that the
  synonym names with `@` can only be given a variable or `_`.

How `forall` inside a type works: where a function's parameter has the
type `forall a. t`, the argument is inferred with `a` standing for itself
and becomes a function of `a`; each use of the parameter gives it a type.
A `forall` on the result of a function is moved to the front of the
signature, which means the same. That is also how a record field with a
`forall` type is used as a function: its type variables are the
selector's.

How derived `Functor`, `Foldable` and `Traversable` work: all three go
through the values of the last type parameter in the fields of a value.
The interpreter says where those are in each field (a field `[(op, a)]`:
inside the list, inside the pair), and one function in
`lib/Interpreter/Derive.hs` does the walk as `traverse`. `fmap` and
`foldr` are that `traverse` with an `Applicative` that only applies the
function, or only collects the values.

How `eqT` works: every type is at hand when the program runs, so `eqT @a
@b` compares the two. A match on its `Refl` is where the program relies on
the answer, and the type pass has to as well: inside that alternative, a
type variable that does not fit is tried as the type it was shown to be
(the given types in `src/infer.c`).

What the type pass does not check: the constraints of a signature. A class
with two parameters, of which only the first decides the instance, can so
be written with one (`MVector v a` in `lib/Data/Vector/Generic/Mutable.hs`).

Still missing, as the survey shows them:

- Derived `Read`; `deriving via` for anything but `Semigroup` and `Monoid`
  via `Generically`. (`Read` is there for numbers,
  characters, strings, lists and a few small types: `tests/read.hs`.)
- The methods of `Data` other than `gmapT`, `gmapQ` and `gmapM`, for a
  derived instance. `Generic` is a class with nothing in it.
- `MultiWayIf`; multi-parameter classes (one use, `MonadState`).

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
