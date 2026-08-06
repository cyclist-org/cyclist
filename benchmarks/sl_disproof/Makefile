TIMEOUT:=$(or $(TIMEOUT),60)
JOBS:=$(or $(JOBS),4)
SEQS:=$(or $(SEQS),seqs)
DEFS:=$(or $(DEFS),all.defs)
RESULTS:=$(or $(RESULTS),results)
CMD := src/seplog/disprove.exe
# Normally exported by the top-level Makefile; defaulted so that a bad PROVER
# path cannot silently turn every query into an "unknown".
ROOT:=$(or $(ROOT),$(realpath $(CURDIR)/../..))

# Only one dune instance can hold the build lock, so the workers below cannot
# each go through `dune exec`; build once here and pass the binary down instead.
PROVER := $(ROOT)/_build/default/$(CMD)

export PROVER TIMEOUT DEFS RESULTS

.PHONY: all clean

# One sequent per line in $(SEQS), each dispatched to ./run-inv-test.sh, which
# writes results/<md5 of sequent>.{log,out}. That is ~900k sequents by default,
# at up to $(TIMEOUT)s each, so override SEQS to sweep a subset.
all:
	@cd "$(ROOT)" && dune build $(CMD)
	@mkdir -p $(RESULTS)
	@echo "sweeping $$(wc -l < $(SEQS)) sequents from $(SEQS), $(JOBS) at a time"
	@tr '\n' '\0' < $(SEQS) | nice xargs -n 1 -P $(JOBS) -0 ./run-inv-test.sh

clean:
	rm -rf $(RESULTS)
