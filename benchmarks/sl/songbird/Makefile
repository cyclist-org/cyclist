TIMEOUT:=$(or $(TIMEOUT),30)
TESTS:= $(sort $(wildcard *.tst))
DEFS := "$(ROOT)/examples/songbird.defs"

.PHONY: all $(TESTS)
all: $(TESTS)

$(TESTS): %.tst:
	@echo "$@"
	-@while read -r SEQ; do \
		echo -n "\t"; \
		(cd "$(ROOT)" && timeout "$$(($(TIMEOUT) + 1))" dune exec $(CMD) -- $(TST_OPTS) -t $(TIMEOUT) -S "$$SEQ" || echo "Not proved (timeout)") ;\
	done < "$@"
