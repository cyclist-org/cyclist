ROOT := $(shell pwd)
DEFS := $(ROOT)/examples/sl.defs

export ROOT DEFS

all:
	dune build

clean:
	dune clean

# Reformat all OCaml and dune sources in place.
fmt:
	dune fmt

# Fail if anything is unformatted, without touching the tree (for CI).
fmt-check:
	dune build @fmt

# Build the API documentation into _build/default/_doc/_html.
doc:
	dune build @doc

# Install the repo's git hooks (once per clone).
hooks:
	git config core.hooksPath .githooks

.PHONY: all clean fmt fmt-check doc hooks

%-tests:
	$(MAKE) -C benchmarks $*
