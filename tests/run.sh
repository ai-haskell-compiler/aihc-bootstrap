#!/bin/sh
# Runs every tests/NAME.hs and compares its output with tests/NAME.stdout.
# Generate the .stdout files with GHC (see README.md). If tests/NAME.args
# exists, its words are the command line of the program.
set -eu

status=0
for source in tests/*.hs; do
	expected="${source%.hs}.stdout"
	arguments=""
	if [ -f "${source%.hs}.args" ]; then
		arguments=$(cat "${source%.hs}.args")
	fi
	# The arguments are meant to be split into words.
	# shellcheck disable=SC2086
	if ./build/aihc-bootstrap -i lib "$source" $arguments | cmp -s - "$expected"; then
		echo "ok   $source"
	else
		echo "FAIL $source"
		status=1
	fi
done
exit "$status"
