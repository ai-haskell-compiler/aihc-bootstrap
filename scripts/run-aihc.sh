#!/bin/sh
# Runs AIHC in the interpreter. The arguments are AIHC's own command line.
# AIHC_BOOTSTRAP_FLAGS=--survey reports what is unsupported instead of running.
#
# The source directories are those of the aihc library and its first-party
# dependencies, with the hackage and pretty-ui flags off. A directory after
# +OverloadedStrings has that extension on, as the package's .cabal file
# says. AIHC is expected beside this repository in aihc, with aihc-parser, aihc-cpp and
# aihc-cabal-syntax beside it; set AIHC to use another checkout.
set -eu

# The script can be run from any directory: AIHC looks for its core
# libraries from the directory it is started in.
here=$(cd "$(dirname "$0")/.." && pwd)
aihc=$(cd "${AIHC:-$here/../aihc}" && pwd)
directories="
$here/lib
$here/glue
+OverloadedStrings
$aihc/bin/aihc/src
$aihc/bin/aihc/prim-wiring
$aihc/bin/aihc/compiler/amd64/src
$aihc/bin/aihc/compiler/arm64/src
$aihc/bin/aihc/compiler/fc/src
$aihc/bin/aihc/compiler/grin/src
$aihc/bin/aihc/compiler/lir/src
$aihc/bin/aihc/compiler/llvm/src
$aihc/bin/aihc/compiler/native/src
$aihc/bin/aihc/compiler/th/src
$aihc/bin/aihc/compiler/wasm/src
$aihc/bin/aihc/hackage/disabled
$aihc/bin/aihc/pretty-ui/disabled
$aihc/tooling/aihc-package-plan/src
$aihc/tooling/aihc-package-plan/src-not-embedded
-OverloadedStrings
$aihc/components/aihc-resolve/src
$aihc/components/aihc-tc/src
$aihc/tooling/aihc-hackage/src
$aihc/../aihc-parser/src
$aihc/../aihc-cpp/src
$aihc/../aihc-cabal-syntax/src
"

# AIHC looks for its data files (run-time sources, linker scripts) here.
aihc_datadir="$aihc/bin/aihc"
export aihc_datadir

# Build the command line back to front, so that it ends up in this order.
set -- "$aihc/bin/aihc/exe/Main.hs" "$@"
reversed=""
for directory in $directories; do
	reversed="$directory $reversed"
done
for directory in $reversed; do
	case "$directory" in
	+*) set -- -X "${directory#+}" "$@" ;;
	-*) set -- -X "No${directory#-}" "$@" ;;
	*) set -- -i "$directory" "$@" ;;
	esac
done
# shellcheck disable=SC2086
exec "$here/build/aihc-bootstrap" ${AIHC_BOOTSTRAP_FLAGS:-} "$@"
