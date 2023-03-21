

build:
	@dune build @all @install

all: build

clean:
	@dune clean

test:
	@dune runtest --no-buffer -f

fmt:
	@dune build @fmt --auto-promote

WATCH?= @all
watch:
	@dune build $(WATCH) -w

install: build
	@dune install

reindent:
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -print0 | xargs -0 echo "reindenting: "
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -print0 | xargs -0 sed -i 's/[[:space:]]*$$//'
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -print0 | xargs -0 ocp-indent -i

.PHONY: all clean watch
