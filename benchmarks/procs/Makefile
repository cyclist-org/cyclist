TESTS:=$(sort $(shell find . -name '*.wl2'))
CMD := procedure_prove

.PHONY: all $(TESTS)
all: $(TESTS)

$(TESTS): %.wl2:
	@echo $@
	@dune exec $(CMD) -- $(TST_OPTS) -D $(DEFS) -all -P $@ || true
	@echo
