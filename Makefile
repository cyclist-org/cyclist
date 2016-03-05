OCB := ocamlbuild -j 8

TARBALL:=cyclist.tar.gz
BENCHDIR:=benchmarks

#FOMAIN:=./src/firstorder/fo_prove.native
SLMAIN:=./src/seplog/sl_prove.native
PRMAIN:=./src/goto/goto_prove.native
PR2MAIN:=./src/while/while_prove.native
XTDPRMAIN:=./src/extended_while/extended_while_prove.native
ABD2MAIN:=./src/while/while_abduce.native
TEMPORALMAIN:=./src/temporal/temporal_prove.native

all:
	$(OCB) all.otarget

.PHONY: tests
tests:
	$(OCB) -no-links tests.otarget

check: tests
	@for TST in _build/tests/test_*.native ; do $$TST ; done

%.native: 
	$(OCB) "$@"

%.byte:
	$(OCB) "$@"

clean:
	$(OCB) -clean

#fo-tests:
#	-@for TST in $(BENCHDIR)/fo/*.tst ; do _build/$(FOMAIN) $(TST_OPTS) -S "`cat $$TST`" ; done

sl-tests:
	-@for TST in $(BENCHDIR)/sl/*.tst ; do _build/$(SLMAIN) $(TST_OPTS) -S "`cat $$TST`" ; done

goto-tests:
	-@for TST in $(BENCHDIR)/goto/*.tc ; do _build/$(PRMAIN) $(TST_OPTS) -P $$TST ; done

whl-tests:
	-@for TST in $(BENCHDIR)/whl/*.wl ; do echo $$TST: ; _build/$(PR2MAIN) $(TST_OPTS) -P $$TST ; echo ; done

xsf-tests:
	-@for TST in $(BENCHDIR)/sf/*.wl2 ; do echo $$TST: ; _build/$(XTDPRMAIN) $(TST_OPTS) -P $$TST ; echo ; done

whl_abd-tests:
	-@for TST in $(BENCHDIR)/whl_abd/*.wl ; do echo $$TST ; _build/$(ABD2MAIN) $(TST_OPTS) -P $$TST ; echo ; done

aplas-tests: sl-tests #goto-tests #fo-tests 

tp-tests: sl-tests whl-tests xsf-tests #fo-tests

abd-tests: whl_abd-tests

all-tests: tp-tests abd-tests
