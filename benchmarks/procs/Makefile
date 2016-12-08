TESTS:=$(sort $(shell find . -name '*.wl2'))
XTDPRMAIN:=$(ROOT)/_build/src/procedure/procedure_prove.native

.PHONY: all $(TESTS)
all: $(TESTS)

$(TESTS): %.wl2:
	@echo $@
	@$(XTDPRMAIN) $(TST_OPTS) -D $(DEFS) -all -P $@ || true
	@echo
