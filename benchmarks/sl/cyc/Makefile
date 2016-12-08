TESTS:= $(sort $(wildcard *.tst))

.PHONY: sl-tests $(TESTS)
sl-tests: $(TESTS)

$(TESTS): %.tst:
	@echo "$@"
	@while read -r SEQ; do \
		echo -n "\t"; \
		../../../_build/$(SLMAIN) -D ../../../examples/sl.defs $(TST_OPTS) -S "$$SEQ" ;\
	done < "$@"


