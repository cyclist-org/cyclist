TARGETS := $(filter-out Makefile,$(wildcard *))

.PHONY: $(TARGETS)

sl-%:
	$(MAKE) -C sl $*

proc-tests:
	-@for TST in $(WHL2_TEST_FILES) ; do echo $$TST: ; _build/$(XTDPRMAIN) $(TST_OPTS) -all -P $$TST ; echo ; done

ctl-tests:
	-@for TST in $(CTL_TEST_FILES) ; do echo $$TST: ; _build/$(CTLMAIN) $(TST_OPTS) -P $$TST ; echo ; done

ltl-tests:
	-@for TST in temporal/ltl/*.ltl ; do echo $$TST: ; _build/$(LTLMAIN) $(TST_OPTS) -P $$TST ; echo ; done

aplas-tests: sl-tests #goto-tests #fo-tests

tp-tests: sl-tests whl-tests xsf-tests #fo-tests

abd-tests: whl_abd-tests

all-tests: tp-tests abd-tests

$(TARGETS): %:
	$(MAKE) -C $@ all
