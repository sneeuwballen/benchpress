

build:
	@dune build @install

all: build

clean:
	@dune clean

watch:
	@dune build @all -w

.PHONY: all clean watch
