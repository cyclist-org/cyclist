TESTS:=$(sort $(shell find . -name '*.wl2'))
CMD := cyclist
SUB := proc prove

.PHONY: all $(TESTS)
all: $(TESTS)

$(TESTS): %.wl2:
	@echo $@
	@dune exec $(CMD) -- $(SUB) $(TST_OPTS) -D $(DEFS) --all -P $@ || true
	@echo
