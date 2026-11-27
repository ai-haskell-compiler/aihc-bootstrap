#!/bin/sh
# Stands for a source preprocessor (alex, happy, c2hs) that the bootstrap
# does not have, so that one on the PATH, which GHC built, is not used.
echo "the bootstrap has no alex, happy or c2hs: $*" >&2
exit 1
