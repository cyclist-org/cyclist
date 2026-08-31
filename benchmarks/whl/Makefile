TESTS:= $(sort $(wildcard *.wl))
CMD := cyclist
SUB := while prove

.PHONY: all $(TESTS)
all: $(TESTS)

$(TESTS): %.wl:
	@echo "$@"
	-@echo -n "\t" ; dune exec $(CMD) -- $(SUB) $(TST_OPTS) -D $(DEFS) -P $@
