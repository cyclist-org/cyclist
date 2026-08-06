TESTS:= $(sort $(wildcard *.wl))
CMD := while_abduce

.PHONY: all $(TESTS)
all: $(TESTS)

$(TESTS): %.wl:
	@echo "$@"
	-@echo -n "\t" ; dune exec $(CMD) -- $(TST_OPTS) -P $@
