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

# Trace the proofs of the benchmark cases in benchmarks/proof-trace.cases, for
# comparison with the committed baselines; see benchmarks/proof-trace.py. Each
# benchmark directory has a baseline proof-trace.log of its own, and gets its
# new trace in proof-trace.new.log. The targets cover every directory, or with
# a prefix, as in `make sl-base-check-trace`, only the one that <base>-tests
# would run: fo, sl for all of sl/, sl-base for sl/base, and so on.
PROOF_TRACE := python3 benchmarks/proof-trace.py
# The benchmark directory named by the stem of a <base>- target.
trace_dir = $(patsubst sl-%,sl/%,$*)

# Write the traces.
proof-trace:
	$(PROOF_TRACE) run
%-proof-trace:
	$(PROOF_TRACE) run $(trace_dir)

# Write the traces and print how they differ from the baselines, failing if
# they do. Only the directories are reported as they are reached.
check-trace:
	@$(PROOF_TRACE) check
%-check-trace:
	@$(PROOF_TRACE) check $(trace_dir)

# Accept the last traces as the baselines.
replace-trace:
	$(PROOF_TRACE) replace
%-replace-trace:
	$(PROOF_TRACE) replace $(trace_dir)

# Remake the choice of cases for proof-trace. This runs every benchmark case,
# so it takes a while, and its result depends on the speed of the host.
proof-trace-select:
	$(PROOF_TRACE) select -o benchmarks/proof-trace.cases

.PHONY: all clean fmt fmt-check doc hooks proof-trace check-trace replace-trace proof-trace-select

%-tests:
	$(MAKE) -w -C benchmarks $*
