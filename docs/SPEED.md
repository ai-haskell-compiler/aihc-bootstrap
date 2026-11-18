# Plan: a much faster interpreter

The goal is that AIHC, run in the interpreter, compiles itself in hours or
less, not days. Today it is about 200 times too slow for that.

## Where we are

| Build of a hello world (`aihc build`) | CPU | Instructions |
| --- | ---: | ---: |
| AIHC built with GHC | 24 s | 270 G |
| AIHC in the interpreter (run 15, `efaaf0b`) | 5,174 s | |
| Ratio | about 210× | |

The `-O` level makes no difference to this build: `-O0` costs what the
default costs. The plan compiles 325 modules (1.9 MB of source). AIHC
itself is about 460 modules (7.8 MB: `bin/aihc`, `components`, and the
three parser libraries), and self-compiling also builds about 130
Hackage packages first (`docs/self-hosting-packages.md` in AIHC). That is
probably 5 to 10 times the work of the hello world plan: 7 to 15 hours of
CPU at today's speed, if compile time grows with the amount of source.

To get under an hour, the interpreter has to be 10 to 20 times faster.
Running packages in parallel (see the last section) shortens the wall
time further, but does not make the interpreter faster.

## Why it is slow

The profile is flat. No single function is the problem: the costs come
from the design.

- **It walks the syntax tree.** An `Expr` is a 384-byte node for every
  kind; much of the time goes to cache misses on the first look at an
  expression or a pattern.
- **Variables are found by name and shape at run time.** An environment
  is a flat array with a shape that is looked up in a hash table; every
  closure, thunk and pattern copies the variables it keeps into a new
  array (`keep`, `bind_waiting`, `bound`). Around 20 million environments
  are made to parse a 28 kB module, more than values.
- **Everything is lazy and boxed.** Every argument that is not a variable
  or a literal becomes a thunk plus an environment, even when the
  function (`+`, `<`, `compare`) evaluates it at once. An `Int` outside
  -128 to 1023 is a heap value.
- **Types are passed at run time** and class methods are looked up by
  type (`find_method`, `close_type`): cached now, still a few percent.
- **The machine recurses in C.** Builtins, nested patterns and derived
  methods evaluate with a nested call of the machine. The C stack can
  hold a pointer to anything, so the collector cannot be precise.
- **The collector cannot move anything**, so allocation is a free list
  plus zeroing (about 20 cycles a cell) and sweeping visits every dead
  young cell. Allocating and collecting is about 30% of the time.

GHCi's bytecode interpreter, which has none of these problems, is
roughly 10 to 30 times slower than optimised native code. That is about
the best a good interpreter does; it is also about the 10× we need.

## The plan

Each phase is measured against the same benchmarks before it is kept:
the parse benchmark (114 kB module), the map benchmark, `aihc-rts` byte
for byte, and from time to time the whole hello world plan, compared
with the store of AIHC built with GHC.

### Phase 1: measure by phase (days)

Find the parts of AIHC where the interpreter is far slower than its
average. The executable `Main` (two lines) took 32 of the 86 minutes of
run 15, but 1.6 of 11 seconds natively: reading interface files or
linking is out of proportion, and may have a cheap fix in a builtin or a
library replacement. Also:

- A benchmark of one minute or less that goes through all of AIHC's
  phases: rebuilding `Main` against a store that already has `aihc-prim`
  and `aihc-base`.
- The time of each AIHC phase (parse, rename, type check, desugar, code
  generation, interface files), interpreted and native.

Expected: a few targeted wins of 1.2 to 2× on the whole build.

### Phase 2: compile before running (weeks; 3 to 5×)

A pass between type checking and evaluation that turns each top-level
definition into a small core language, and a new machine for it. This is
the largest step, and the base for everything after it:

- **Closure conversion with fixed slots.** Every lambda and thunk knows
  its free variables when it is compiled, and every variable is a slot
  number: a local, an argument or a free variable. No shapes, no lookups
  by binding, no `keep`. A thunk or closure is one allocation, with its
  free variables inside it.
- **Functions take all their arguments at once.** `\a -> \b -> e` is a
  function of two arguments; a call to a known function with all of them
  goes straight to its body. Partial applications only when they are
  real.
- **Patterns compiled to decision trees.** A `case` switches on a tag;
  nested patterns become nested cases in the machine, with no recursion
  in C and no matcher walking pattern trees.
- **A compact code representation**: small nodes or bytecode laid out
  per function, instead of 384-byte syntax nodes spread over memory.

The library replacements in `lib/` and the builtins stay as they are.

### Phase 3: values and memory (weeks; 1.5 to 2.5×)

- **Small integers and characters inside the pointer** (tagged), so that
  arithmetic does not allocate.
- **No recursion in C.** Builtins that evaluate their arguments ask the
  machine to do it (a frame that continues the builtin), and derived
  methods become ordinary code. Then the machine's stack and its
  registers are all the roots there are, and they are known exactly.
- **A copying nursery.** With exact roots, young values can be moved:
  allocation is a pointer increment, nothing dead is visited, and what
  survives is compacted. The old generation can stay mark and sweep.

### Phase 4: less laziness and fewer types (weeks; 1.3 to 2×)

- **Strict arguments without thunks.** The arguments of builtins that
  always evaluate them, and the arguments a simple analysis finds a
  function always evaluates, are evaluated before the call instead of
  being delayed.
- **Types only where they are needed.** A type argument that no method
  lookup or derived method reads is not passed. Methods at types known
  when the code is compiled are found then.

Phases 2 to 4 together: about 10× (estimates; each phase is measured
before the next).

### Phase 5, if 10× is not enough: compile to C (months; another 3 to 10×)

The core language of phase 2 is also enough to generate C: one C
function per top-level definition, run by the same runtime (collector,
builtins). Everything the interpreter would run (AIHC and `lib/`) is
compiled once into one program with Clang. GHC's old C backend was within
a factor of 2 of its native code generator; something like it would put
a self-compile in minutes. Its costs: all of AIHC has to be type checked
before anything runs (today definitions are prepared on first use), and
compiling the generated C takes a while once per change to AIHC.

This is the only route to 100×. Phase 2 is needed for it either way.

## Wall time: run packages in parallel

Independent of all this: the interpreter is single threaded, and the
machine has 12 cores. Building several of the ~130 packages at once
(`aihc install` of independent packages into the same store, in
dependency order) cuts the wall time by up to the number of cores. It
costs a script, not interpreter work.
