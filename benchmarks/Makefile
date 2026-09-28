TARGETS := $(patsubst %/,%,$(wildcard */))
# Normally exported by the top-level Makefile.
ROOT := $(or $(ROOT),$(realpath $(CURDIR)/..))
DEFS := $(or $(DEFS),$(ROOT)/examples/sl.defs)
export ROOT DEFS

.PHONY: $(TARGETS) cases

# The families that proof-trace.py draws its test cases from. Left out are
# asl_whl, whose results depend on the installed z3, and sl_disproof and slmc,
# which are sweeps and experiments rather than test suites.
PROOF_TRACE := fo ltl procs sl slsat soundness whl whl_abd

sl-%:
	$(MAKE) -w -C sl $*

$(TARGETS): %:
	$(MAKE) -w -C $@ all

# One line per test case, for proof-trace.py; see cases.mk.
cases:
	@for d in $(PROOF_TRACE); do $(MAKE) -s --no-print-directory -C $$d cases || exit 1; done
