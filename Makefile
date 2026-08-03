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

# Install the repo's git hooks (once per clone).
hooks:
	git config core.hooksPath .githooks

.PHONY: all clean fmt fmt-check hooks

%-tests:
	$(MAKE) -C benchmarks $*
