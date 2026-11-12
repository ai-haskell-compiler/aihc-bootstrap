CC = cc
CFLAGS = -std=c89 -pedantic -Wall -Wextra -Werror -g

SOURCES = src/util.c src/lexer.c src/module.c src/parse.c src/resolve.c \
          src/types.c src/infer.c src/instances.c src/derive.c src/gc.c src/eval.c src/numbers.c src/integer.c src/bytes.c src/files.c src/sha256.c src/md5.c src/unicode.c src/posix.c src/builtins.c src/elaborate.c src/survey.c src/main.c
HEADERS = src/util.h src/lexer.h src/syntax.h src/eval.h src/unicode_table.h

build/aihc-bootstrap: $(SOURCES) $(HEADERS)
	mkdir -p build
	$(CC) $(CFLAGS) -o $@ $(SOURCES) -lm -lpthread

.PHONY: test test-gc survey clean

test: build/aihc-bootstrap
	sh tests/run.sh

# The tests again, with a collection after every 100 kB that is allocated:
# a value that the collector frees too early is then found at once.
test-gc: build/aihc-bootstrap
	AIHC_GC_EVERY=100000 sh tests/run.sh

# What AIHC needs that the interpreter does not have yet (src/survey.c).
survey: build/aihc-bootstrap
	AIHC_BOOTSTRAP_FLAGS=--survey sh scripts/run-aihc.sh

clean:
	rm -rf build
