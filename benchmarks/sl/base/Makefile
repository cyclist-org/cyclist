TIMEOUT:=$(or $(TIMEOUT),30)
TESTS:= $(sort $(wildcard *.tst))

.PHONY: all $(TESTS)

all: $(TESTS)

$(TESTS): %.tst:
	-@TST_COUNT=1; \
	while read -r SEQ; do \
		echo "$@.$$TST_COUNT"; \
		TST_COUNT=$$(($$TST_COUNT+1)); \
		(cd "$(ROOT)" && timeout "$$(($(TIMEOUT) + 1))" dune exec $(CMD) -- $(TST_OPTS) -t $(TIMEOUT) -S "$$SEQ" || echo "Not proved (timeout)") ;\
	done < "$@"
