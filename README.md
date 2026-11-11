# aihc-bootstrap

A Haskell interpreter in C89 whose only job is to run
[AIHC](https://github.com/ai-haskell-compiler/aihc), so that AIHC can be
bootstrapped from a C compiler.

## Rules

- As simple as possible. Readability always wins over performance.
- Support only the Haskell that AIHC and its dependencies need.
- Strict C89 (`-std=c89 -pedantic -Wall -Wextra -Werror`).
- No readable error messages, no defence against bad input.

## How it grows

```sh
make
sh scripts/run-aihc.sh build foo.hs
```

The interpreter runs AIHC's `main` until it meets something it does not
support and then stops with a line such as:

```text
module GHC.Clock not found
```

Implement that, add a small test for it, and run again.

`make survey` gives the whole picture instead of the first failure. It
prepares every definition AIHC could reach, without running anything, and
lists what is unsupported, most frequent first. Its first lines are the
measure of progress:

```text
modules loaded:        310
definitions prepared:  9146 of 11737
```

The order of the library work is in [docs/PLAN.md](docs/PLAN.md).

Work is done lazily, per top-level declaration. Loading a module lexes it and
records where each declaration is. A declaration is parsed, resolved and typed
the first time its value is needed. Code that AIHC never runs is never looked
at, so unused features in dependencies do not have to be implemented.

## Type classes without dictionaries

Types are passed at run time. The type pass (`src/infer.c`) does not look for
type errors, since a real compiler has already accepted the program. It finds
out at which types polymorphic things are used and writes that into the
program:

- a use of `f :: forall a. ...` becomes `f @a`, with the type as an ordinary
  argument;
- the definition of `f` becomes a function of its type variables;
- a use of a class method carries the type the class is used at.

At run time a method call matches that type against the heads of the class's
instances (`src/instances.c`). Constraints, instance contexts and superclasses
are therefore never looked at.

## Layout

| File | Purpose |
| --- | --- |
| `src/lexer.c` | Source text to tokens |
| `src/module.c` | Finds modules, splits them into declarations, looks up names |
| `src/parse.c` | Tokens of one declaration to an `Expr` |
| `src/resolve.c` | Replaces names by pointers to what they refer to; operator precedence |
| `src/types.c` | Types, the type parser, and type schemes |
| `src/infer.c` | Type inference; makes type arguments explicit |
| `src/instances.c` | Finds the instance for a method call at run time |
| `src/derive.c` | The methods of instances that come from `deriving` |
| `src/numbers.c` | Arithmetic for every built-in number type |
| `src/bytes.c` | `ByteString`, and reading and writing files |
| `src/elaborate.c` | The pipeline for one declaration; new passes are added here |
| `src/eval.c` | The evaluator: a machine with an explicit stack |
| `src/builtins.c` | C functions reachable through `foreign import prim` |
| `src/survey.c` | Survey mode: what is unsupported, without running |
| `lib/TopHandler.hs` | What runs `main` and handles exceptions it does not catch |
| `lib/` | The interpreter's own base library, in Haskell, and small replacements for third-party packages (`lib/Options/Applicative.hs`) |
| `tests/` | `NAME.hs` with the expected output in `NAME.stdout` |

## Tests

```sh
make test
```

Write the expected output with GHC, so that the interpreter is always compared
against a real compiler:

```sh
runghc --ghc-arg=-package-env --ghc-arg=- tests/NAME.hs > tests/NAME.stdout
```
