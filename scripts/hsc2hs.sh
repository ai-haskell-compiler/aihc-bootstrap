#!/bin/sh
# Runs hsc2hs in the interpreter. The arguments are hsc2hs's own command
# line. AIHC runs it for each .hsc file when AIHC_HSC2HS names this script.
#
# HSC2HS_SOURCES is the directory of the hsc2hs package from Hackage, with
# its sources already through the C preprocessor in pp/
# (scripts/bootstrap.py prepares it). Its data file, template-hsc.h, is in
# data/.
set -eu

here=$(cd "$(dirname "$0")/.." && pwd)
sources=${HSC2HS_SOURCES:?HSC2HS_SOURCES names the prepared hsc2hs package}
hsc2hs_datadir="$sources/data"
export hsc2hs_datadir
exec "$here/build/aihc-bootstrap" -i "$here/lib" -i "$here/glue/hsc2hs" \
	-i "$sources/pp" "$sources/pp/Main.hs" "$@"
