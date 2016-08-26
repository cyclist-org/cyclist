OCB := ocamlbuild -j 8 -ocamldoc "ocamldoc -hide-warnings"

TARBALL:=cyclist.tar.gz
BENCHDIR:=benchmarks

#FOMAIN:=./src/firstorder/fo_prove.native
SLMAIN:=./src/seplog/sl_prove.native
PRMAIN:=./src/goto/goto_prove.native
PR2MAIN:=./src/while/while_prove.native
XTDPRMAIN:=./src/extended_while/extended_while_prove.native
ASLMAIN:=./src/asl_while/asl_while_prove.native
ABD2MAIN:=./src/while/while_abduce.native
CTLMAIN:=./src/temporal_ctl/temporal_ctl_prove.native
LTLMAIN:=./src/temporal_ltl/temporal_ltl_prove.native
Z3INSTALL:=./_build/z3

all: _build/z3
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

%.otarget:
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

asl_whl-tests:
	-@for TST in $(BENCHDIR)/asl_whl/*.aslwl ; do echo $$TST: ; _build/$(ASLMAIN) $(TST_OPTS) -P $$TST ; echo ; done

aplas-tests: sl-tests #goto-tests #fo-tests 

tp-tests: sl-tests whl-tests xsf-tests #fo-tests

abd-tests: whl_abd-tests

all-tests: tp-tests abd-tests

_build/z3:
	if [ ! -d $(Z3INSTALL) ]; \
	then \
		git clone https://github.com/Z3Prover/z3.git $(Z3INSTALL); \
	fi
	cd $(Z3INSTALL)                                      && \
		python scripts/mk_make.py --prefix=../..         && \
		cd build                                         && \
		$(MAKE)                                          && \
		$(MAKE) install
