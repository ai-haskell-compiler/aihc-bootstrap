#!/bin/sh
# Runs AIHC in the interpreter. The arguments are AIHC's own command line.
# AIHC_BOOTSTRAP_FLAGS=--survey reports what is unsupported instead of running.
#
# The source directories are those of the aihc library and its first-party
# dependencies, with the hackage and pretty-ui flags off. A directory after
# +OverloadedStrings has that extension on, as the package's .cabal file
# says. AIHC is expected in ../aihc, with aihc-parser, aihc-cpp and
# aihc-cabal-syntax beside it; set AIHC to use another checkout.
set -eu

aihc="${AIHC:-../aihc}"
directories="
lib
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
-OverloadedStrings
$aihc/components/aihc-resolve/src
$aihc/components/aihc-tc/src
$aihc/tooling/aihc-hackage/src
$aihc/../aihc-parser/src
$aihc/../aihc-cpp/src
$aihc/../aihc-cabal-syntax/src
"

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
exec ./build/aihc-bootstrap ${AIHC_BOOTSTRAP_FLAGS:-} "$@"
