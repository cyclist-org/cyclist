CTLMAIN:=$(ROOT)/_build/src/temporal_ctl/temporal_ctl_prove.native
LTLMAIN:=$(ROOT)/_build/src/temporal_ltl/temporal_ltl_prove.native

LTLTARGETS:=$(wildcard ltl/*.ltl)
CTLTARGETS:=$(sort $(shell find . -name '*.ctl'))
.PHONY: ltl ctl $(LTLTARGETS) $(CTLTARGETS)
ltl: $(LTLTARGETS)
ctl: $(CTLTARGETS)

$(LTLTARGETS): %.ltl:
	@echo $@
	@echo -n "\t" ; $(LTLMAIN) $(TST_OPTS) -D $(DEFS) -P $@ || true
	@echo

$(CTLTARGETS): %.ctl:
	@echo $@
	@echo -n "\t" ; $(CTLMAIN) $(TST_OPTS) -D $(DEFS) -P $@ || true
	@echo
