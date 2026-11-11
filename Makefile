CC = cc
CFLAGS = -std=c89 -pedantic -Wall -Wextra -Werror -g

SOURCES = src/util.c src/lexer.c src/module.c src/parse.c src/resolve.c \
          src/types.c src/infer.c src/instances.c src/derive.c src/eval.c src/numbers.c src/bytes.c src/posix.c src/builtins.c src/elaborate.c src/survey.c src/main.c
HEADERS = src/util.h src/lexer.h src/syntax.h src/eval.h

build/aihc-bootstrap: $(SOURCES) $(HEADERS)
	mkdir -p build
	$(CC) $(CFLAGS) -o $@ $(SOURCES)

.PHONY: test survey clean

test: build/aihc-bootstrap
	sh tests/run.sh

# What AIHC needs that the interpreter does not have yet (src/survey.c).
survey: build/aihc-bootstrap
	AIHC_BOOTSTRAP_FLAGS=--survey sh scripts/run-aihc.sh

clean:
	rm -rf build
