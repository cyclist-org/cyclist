TESTS:= $(sort $(wildcard *.wl))
CMD := $(ROOT)/_build/src/while/while_prove.native

.PHONY: all $(TESTS)
all: $(TESTS)

$(TESTS): %.wl:
	@echo "$@"
	-@echo -n "\t" ; $(CMD) $(TST_OPTS) -D $(DEFS) -P $@
