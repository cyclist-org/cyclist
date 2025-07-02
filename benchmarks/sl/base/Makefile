TIMEOUT:=$(or $(TIMEOUT),30)
TESTS:= $(sort $(wildcard *.tst))

.PHONY: all $(TESTS)

all: $(TESTS)

$(TESTS): %.tst:
	-@TST_COUNT=1; \
	TST_NAME=$@; \
	while read -r SEQ; do \
		TST_CASE="cyclist-sl-$${TST_NAME%.tst}.$$TST_COUNT"; \
		echo "test case: $$TST_CASE"; \
		TST_COUNT=$$(($$TST_COUNT+1)); \
		(cd "$(ROOT)" && timeout "$$(($(TIMEOUT) + 1))" dune exec $(CMD) -- -t $(TIMEOUT) -S "$$SEQ" --id="$$TST_CASE" $(TST_OPTS) || echo "Not proved (timeout)"); \
	done < "$@"
