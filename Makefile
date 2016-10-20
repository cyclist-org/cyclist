OCB := ocamlbuild -j 8 -ocamldoc "ocamldoc -hide-warnings"

BENCHDIR:=benchmarks

#FOMAIN:=./src/firstorder/fo_prove.native
SLMAIN:=./src/seplog/sl_prove.native
#PRMAIN:=./src/goto/goto_prove.native
PR2MAIN:=./src/while/while_prove.native
XTDPRMAIN:=./src/procedure/procedure_prove.native
ABD2MAIN:=./src/while/while_abduce.native
CTLMAIN:=./src/temporal_ctl/temporal_ctl_prove.native
LTLMAIN:=./src/temporal_ltl/temporal_ltl_prove.native

WHL2_TEST_FILES:=$(shell find ./benchmarks/procs -name '*.wl2' | sort)

all:
	$(OCB) all.otarget

.PHONY: tests
tests:
	$(OCB) -no-links tests.otarget

check: tests
	@for TST in _build/tests/test_*.native ; do $$TST ; done

docs: 
	$(OCB) src/cyclist.docdir/index.html

%.native: 
	$(OCB) "$@"

%.byte:
	$(OCB) "$@"

%.otarget:
	$(OCB) "$@"

clean:
	$(OCB) -clean

#fo-tests:
#	-@for TST in $(BENCHDIR)/fo/*.tst ; do _build/$(FOMAIN) $(TST_OPTS) -S "`cat $$TST`" ; done

sl-tests:
	-@for TST in $(BENCHDIR)/sl/*.tst ; do \
		echo "$$TST"; \
		while read -r SEQ; do \
			echo -n "\t"; \
			 _build/$(SLMAIN) $(TST_OPTS) -S "$$SEQ"; \
		done < $$TST; \
	done

sl-atva-tests:
	-@for TST in $(BENCHDIR)/sl/ATVA-2014/*.tst ; do \
		echo "$$TST"; \
		while read -r SEQ; do \
			echo -n "\t"; \
			 _build/$(SLMAIN) $(TST_OPTS) -D examples/IosifEtAl-ATVA2014.defs -S "$$SEQ"; \
		done < $$TST; \
	done

sl-songbird-tests:
	-@for TST in $(BENCHDIR)/sl/songbird/*.tst ; do \
		echo "$$TST"; \
		while read -r SEQ; do \
			echo -n "\t"; \
			_build/$(SLMAIN) $(TST_OPTS) -D examples/songbird.defs -S "$$SEQ"; \
		done < $$TST; \
	done

goto-tests:
	-@for TST in $(BENCHDIR)/goto/*.tc ; do _build/$(PRMAIN) $(TST_OPTS) -P $$TST ; done

whl-tests:
	-@for TST in $(BENCHDIR)/whl/*.wl ; do echo $$TST: ; _build/$(PR2MAIN) $(TST_OPTS) -P $$TST ; echo ; done

proc-tests:
	-@for TST in $(WHL2_TEST_FILES) ; do echo $$TST: ; _build/$(XTDPRMAIN) $(TST_OPTS) -P $$TST ; echo ; done

whl_abd-tests:
	-@for TST in $(BENCHDIR)/whl_abd/*.wl ; do echo $$TST ; _build/$(ABD2MAIN) $(TST_OPTS) -P $$TST ; echo ; done

aplas-tests: sl-tests #goto-tests #fo-tests 

tp-tests: sl-tests whl-tests xsf-tests #fo-tests

abd-tests: whl_abd-tests

all-tests: tp-tests abd-tests
