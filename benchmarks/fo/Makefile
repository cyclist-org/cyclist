TIMEOUT:=$(or $(TIMEOUT),40)
TESTS:= $(sort $(wildcard *.tst))
DEFS := "$(ROOT)/examples/fo.defs"
CMD := "src/firstorder/prove.exe"

.PHONY: all $(TESTS)

all: $(TESTS)

$(TESTS): %.tst:
	-@TST_COUNT=1; \
	TST_NAME=$@; \
	while read -r SEQ; do \
		TST_CASE="first-order-$${TST_NAME%.tst}.$$TST_COUNT"; \
		echo "test case: $$TST_CASE"; \
		TST_COUNT=$$(($$TST_COUNT+1)); \
		(cd "$(ROOT)" && timeout "$$(($(TIMEOUT) + 1))" dune exec $(CMD) -- -D $(DEFS) -t $(TIMEOUT) -S "$$SEQ" --id="$$TST_CASE" $(TST_OPTS) || echo "Not proved (timeout)"); \
	done < "$@"
