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
| Running AIHC | in progress: type checking `aihc-prim`, see below | |

Left over from phase 1, each stopping with a `TODO` when reached:

- Division by zero as an exception.
- Derived `Read`; `deriving via` other than `Generically`.

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

`Text` is UTF-8 bytes, as upstream: the lexer of `aihc-parser` asks for
the length of the remaining input in bytes at every token.

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

This needs POSIX: ISO C99 has no directories, no `realpath` and no way to
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

`scripts/run-aihc.sh` runs AIHC itself: the AIHC of the submodule
`vendor/aihc`, with its three libraries beside it, each pinned to a
commit. `aihc build /path/to/Main.hs` reads its options, finds the core
libraries, solves the build plan, and builds the packages in order:
`aihc-rts`, `aihc-prim` (27 modules), `aihc-base` (280 modules), then the
program.

**The whole plan builds, and the program runs**: `aihc build` of a hello
world in the interpreter builds `aihc-rts`, `aihc-prim`, `aihc-base` and
the executable, which prints `hello`. **The store is what AIHC built with
GHC writes**, with the same `AIHC_COMPILER_IDENTITY` for both (see below):
every object file (325), library and package file is byte for byte the
same. The unit files under `.units` differ only in the times and the
paths they record, and the `.capi.d` files in the temporary directory
they name.

Each run before that stopped at something the interpreter did not have
yet, in this order: derived `Traversable`, overlapping instances
(`Cbor String` and `Cbor [a]`), `Traversable` of a pair.

The first build that got through took 4 h 53 min (17,097 s of CPU), with
the generational collector but before the rest of "Where the time goes".
With about half of that (up to the arguments bound in one environment)
it took 1 h 52 min (6,684 s), and wrote the same store.
With the changes up to the write barrier that a value answers itself, 1 h
26 min (5,174 s).
AIHC built with GHC builds the whole plan in 34 s of CPU (274 G
instructions): `aihc-prim` 45.7 s and `aihc-base` 44.6 s of wall time.

### Comparing a build with the real one

Build the same program with both into two stores, and `cmp` the files.
Three things have to be the same for the files to be:

- **The sources of AIHC.** The interpreter runs the sources in `vendor/`.
  The AIHC to compare with has to be built from the same commits:
  `cabal build aihc` in `vendor/aihc`, which takes the three libraries at
  the versions that `vendor/` pins (`aihc-parser` 6.0.0.0, `aihc-cpp`
  2.0.0.1, `aihc-cabal-syntax` 2.0.0.0). Point it at the data files of the
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

`sample` on the running interpreter shows it. The machine this ran on was
shared and loaded, so times are compared in cycles and instructions
(`/usr/bin/time -l`), which load does not change. The benchmark is AIHC's
parser on a 28 kB module:

| What | Gcycles |
| --- | ---: |
| At the start, built without optimization | 30 |
| Built with `-O2`; the collector looks only at the pointers of a `Value` and an `Env` | 21 |
| What `find_method` finds for a method at a type without variables is remembered (`src/instances.c`): looking through the instances of the class was a third of the time | 13.7 |
| An environment is an array of values with a shared shape; where a thunk's variables are in an environment of a shape is worked out once (`keep` in `src/eval.c`) | 9.0 |
| A type without variables is not copied by `close_type` | 8.3 |

Earlier, before any of these: each class got a list of its instances, the
block of the last cell of a size is tried first when allocating, and the
search for the block of a word looks only at the bases.

Then the collector became generational (see "Memory"): marking had been
half to two thirds of the time of type checking, and is a few percent.
With that and a few smaller changes the module above took 8.0 Gcycles.

The rest was measured on a 114 kB module (the one above four times), in
the order of the commits:

| What | Gcycles |
| --- | ---: |
| Generational collector | 35.3 |
| A `Value` is 32 bytes instead of 128: its fields share three slots | 28.0 |
| The fields of a constructor are in the cell of its value; a free cell knows where its state is; size classes come from a table | 26.9 |
| A literal, a constructor or a type passed as an argument is made at once, not delayed; a type has one value | 26.3 |
| A lambda that is applied at once binds its argument without making a closure; a builtin takes all the arguments waiting for it at once | 21.8 |
| The arguments waiting for a function are bound in one environment, not one each | 19.7 |
| The variables of a pattern are bound in one environment | 19.6 |
| An expression's type at run time, and the method it stands for, are worked out once (`run_type`, `method_of`) | 18.4 |
| `force` of an evaluated value does not start the machine | 17.8 |
| A constructor without fields is made once; a builtin gets its arguments in an array on the C stack | 17.2 |
| A value says whether it is old, so the write barrier rarely looks it up | 17.0 |
| A thunk that needs all of an environment shares it | 16.7 |

The 28 kB module takes 4.8 Gcycles now. A map benchmark (200,000
inserts) went from 57.5 to 28.6 Gcycles. Derived `Eq` and `Ord` also work
out a constructor's field types, and the methods for them, once
(`src/derive.c`): in the full build, the substitution was 3.6% of the
time.

Tried and not kept: zeroing small cells with stores of a known size,
prefetching the next free cell, finding free cells from the state bytes
instead of a free list (more instructions, no fewer cache misses), and
choosing the alternative of a `case` on an evaluated variable without a
frame.

What is left, from a profile of the full build: the machine itself
(`run`, a fifth), allocating and zeroing cells (a fifth), collecting
(a tenth), `delay` and matching patterns. Arithmetic in a map benchmark
makes a thunk for every argument of `+` and `<`, which the builtin forces
at once; a builtin that is known to force its arguments could have them
evaluated without one.

## Memory

`src/gc.c` frees what the program no longer refers to. The machine is
always inside nested C calls (`force` inside a builtin inside `run`), and
C locals hold values, so the collector does not move anything and reads
the C stack as it is: every word that is the address of something
allocated keeps that alive. It has an allocator of its own, in cells of a
few sizes, so that a word can be checked against what exists. A `Value`
and an `Env` are known to it: only their pointers are looked at.

The collector is generational ("Generations" in `src/gc.c`). A cell that
has been through a collection is old, and most collections (minor ones)
look only at the young cells: what the C stack, the machine's roots and
the old cells that were written to since (the remembered set) point to. A
change to a cell after it is made goes through a write barrier
(`gc_written`, or `gc_written_value`, which a value answers itself). A
cell the C stack points to stays young (it is pinned), because C code
fills in what it has just made without a barrier. A major collection
looks at everything, when the old cells have grown `AIHC_GC_GROWTH` times
(2) since the last one.

`make test-gc` runs the tests with a collection after every 100 kB, every
fourth one major, and freed cells filled with a pattern, so that a cell
used after it was freed goes wrong at once.
`AIHC_GC_REPORT=1` prints a line for every collection, and the lines of
the program that the machine is at. A collection comes at no particular
place, so many of them show where a program spends its time.

These kept memory in use that a compiled program would have let go, and
each had to be fixed for AIHC to run in the memory it has.
`tests/memory.hs` runs into the first three, `tests/strictlet.hs` into the
fourth:

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
4. **`let !x = e` did not evaluate `e`.** aihc-parser takes a position out
   of the parser state with it, so that a declaration does not keep the
   state; the state holds the stream of every token after it. Parsing kept
   all tokens, and `GHC/Types.hs` (435 kB) took 7 minutes and 2.5 GB.
   Also, a thunk lets go of its environment while it is evaluated (and
   raises the same exception again if its evaluation raised one, as in
   GHC), and a `case` keeps only what its alternatives use while its
   scrutinee is evaluated.

What is left, in the order it is likely to matter:

1. **Types made while the program runs are never freed.** A type is made
   whenever a type with variables is passed to a function. In type
   checking that was most of the memory of the process (11 GB) before
   `close_type` stopped copying types without variables.

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
| `let !x = e` and `where !x = e` evaluate `e` before the body | `tests/strictlet.hs` |
| Derived `Functor`, `Foldable`, `Traversable` (by the type of each field) | `tests/derivefunctor.hs` |
| Overlapping instances: the most specific one that matches | `tests/overlap.hs` |

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

How `eqT` works: every type is at hand when the program runs, so `eqT @a
@b` compares the two. A match on its `Refl` is where the program relies on
the answer, and the type pass has to as well: inside that alternative, a
type variable that does not fit is tried as the type it was shown to be
(the given types in `src/infer.c`).

What the type pass does not check: the constraints of a signature. A class
with two parameters, of which only the first decides the instance, can so
be written with one (`MVector v a` in `lib/Data/Vector/Generic/Mutable.hs`).

Still missing, as the survey shows them:

- Derived `Read`; `deriving via` other than `Generically`. (`Read` is there for numbers,
  characters, strings, lists and a few small types: `tests/read.hs`.)
- The methods of `Data` other than `gmapT`, `gmapQ` and `gmapM`, for a
  derived instance. `Generic` is a class with nothing in it.
- `MultiWayIf`; multi-parameter classes (one use, `MonadState`).

## Decisions

Made:

- **POSIX is allowed.** The interpreter stays C99 as a language and may call
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
