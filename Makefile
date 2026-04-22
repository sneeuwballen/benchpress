

build:
	@dune build @all @install

all: build

clean:
	@dune clean

test:
	@dune runtest --no-buffer -f

format:
	@dune build @fmt --auto-promote

format-check:
	@dune build @fmt --display=quiet

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
