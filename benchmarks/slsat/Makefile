TIMEOUT:=$(or $(TIMEOUT),30)
TESTS:= $(sort $(wildcard cc/*.defs))
CMD := "src/seplog/satcheck.exe"

.PHONY: all $(TESTS)

# The random-sampling experiments in syn/ have their own Makefile and are not
# run from here.
all: $(TESTS)

# UNSAT is a normal outcome, and so is the timeout: the succ-circuit and
# succ-rec instances scale up in bit width and the larger ones are meant to be
# out of reach. Hence no failure is reported for a non-zero exit code.
$(TESTS): %.defs:
	-@TST_CASE="slsat-$(notdir $*)"; \
	echo "test case: $$TST_CASE"; \
	(cd "$(ROOT)" && dune exec $(CMD) -- -t $(TIMEOUT) -D "$(CURDIR)/$@" $(TST_OPTS))
