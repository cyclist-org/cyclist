TIMEOUT:=$(or $(TIMEOUT),30)
TESTS:= $(sort $(wildcard *.tst))
CMD := "src/ltl/prove.exe"

.PHONY: all $(TESTS)

all: $(TESTS)

# The test files hold one formula per line, and need not end in a newline.
$(TESTS): %.tst:
	-@TST_COUNT=1; \
	TST_NAME=$@; \
	while read -r SEQ || [ -n "$$SEQ" ]; do \
		TST_CASE="ltl-$${TST_NAME%.tst}.$$TST_COUNT"; \
		echo "test case: $$TST_CASE"; \
		TST_COUNT=$$(($$TST_COUNT+1)); \
		(cd "$(ROOT)" && dune exec $(CMD) -- -t $(TIMEOUT) -S "$$SEQ" --id="$$TST_CASE" $(TST_OPTS) || echo "Not proved"); \
	done < "$@"
