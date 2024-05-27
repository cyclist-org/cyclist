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
		echo "test case: first-order-$${TST_NAME%.tst}.$$TST_COUNT"; \
		TST_COUNT=$$(($$TST_COUNT+1)); \
		(cd "$(ROOT)" && timeout "$$(($(TIMEOUT) + 1))" dune exec $(CMD) -- $(TST_OPTS) -t $(TIMEOUT) -S "$$SEQ" || echo "Not proved (timeout)") ;\
	done < "$@"
