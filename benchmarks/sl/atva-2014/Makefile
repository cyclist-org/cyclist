TIMEOUT:=$(or $(TIMEOUT),30)
TESTS:= $(sort $(wildcard *.tst))
DEFS_FILE := examples/IosifEtAl-ATVA2014.defs
DEFS := "$(ROOT)/$(DEFS_FILE)"

.PHONY: all $(TESTS)
all: $(TESTS)

$(TESTS): %.tst:
	-@TST_COUNT=1; \
	TST_NAME=$@; \
	while read -r SEQ; do \
		TST_CASE="atva-2014-$${TST_NAME%.tst}.$$TST_COUNT"; \
		echo "test case: $$TST_CASE"; \
		TST_COUNT=$$(($$TST_COUNT+1)); \
		(cd "$(ROOT)" && dune exec $(CMD) -- $(SUB) -D $(DEFS) -t $(TIMEOUT) -S "$$SEQ" --id="$$TST_CASE" $(TST_OPTS) || echo "Not proved (timeout)"); \
	done < "$@"

CASES_FROM := lines
CASE_NAME := atva-2014
CASE_ARGS = $(SUB) -D $(DEFS_FILE) -S
include ../../cases.mk
