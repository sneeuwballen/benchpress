

build:
	@dune build @all @install

all: build

clean:
	@dune clean

build-musl:
	@echo "build statically linked version using docker…"
	@docker build --target=build . -t logitest-musl:latest
	@docker cp logitest-musl:latest:/logitest/logitest ./logitest-musl.exe

watch:
	@dune build @all -w

.PHONY: all clean watch
