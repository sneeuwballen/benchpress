

build:
	@dune build @all @install

all: build

build-docker:
	@docker build . -f Dockerfile.server -t benchpress-server:latest

clean:
	@dune clean

test:
	@dune runtest --no-buffer -f

protoc-gen: ## Regenerate protobuf OCaml files from .proto sources
	FORCE_GENPROTO=true dune build @lint

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

HURL ?= hurl
HURL_HOST ?= localhost:8083

test-api: ## Run API auth tests (requires running server; set API_KEY=<key>)
	$(HURL) --variable host=$(HURL_HOST) --variable api_key=$(API_KEY) \
		tests/api_auth.hurl --test

test-api-docker: ## Build and run the self-contained API test container (all-in-one)
	docker build -f Dockerfile.test -t benchpress-test .
	docker run --rm benchpress-test

test-api-compose: ## Run API tests via docker compose (server + runner containers)
	docker compose -f docker/compose.test.yml up \
	    --build --abort-on-container-exit --exit-code-from runner
	docker compose -f docker/compose.test.yml down -v

.PHONY: all clean watch test-api test-api-docker test-api-compose protoc-gen
