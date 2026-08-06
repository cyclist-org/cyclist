TESTS:= $(sort $(wildcard *.wl))
CMD := while_prove

.PHONY: all $(TESTS)
all: $(TESTS)

$(TESTS): %.wl:
	@echo "$@"
	-@echo -n "\t" ; dune exec $(CMD) -- $(TST_OPTS) -D $(DEFS) -P $@
