TESTS:= $(sort $(wildcard *.wl))
DEFS := $(ROOT)/examples/sl.defs
CMD := $(ROOT)/_build/src/while/while_prove.native

.PHONY: all $(TESTS)
all: $(TESTS)

$(TESTS): %.wl:
	@echo "$@"
	-@echo -n "\t" ; $(CMD) $(TST_OPTS) -D $(DEFS) -P $@
