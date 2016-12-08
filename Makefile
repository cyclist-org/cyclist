null  :=
space := $(null) #
comma := ,

PKGS := pcre,melt,unix,str,ocamlgraph,dynlink
TAGS := debug,explain,annot,use_libsoundness
OCAMLDOC := ocamldoc -hide-warnings
INCLUDES := $(subst $(space),$(comma),$(strip $(wildcard src/*)))
OCB := ocamlbuild -use-ocamlfind -j 8 -ocamldoc "$(OCAMLDOC)" -pkgs $(PKGS) -tags $(TAGS) -Is $(INCLUDES)
ROOT := $(shell pwd)

BENCHDIR:=benchmarks

SLMAIN:=./src/seplog/sl_prove.native
PR2MAIN:=./src/while/while_prove.native
XTDPRMAIN:=./src/procedure/procedure_prove.native
ABD2MAIN:=./src/while/while_abduce.native
CTLMAIN:=./src/temporal_ctl/temporal_ctl_prove.native
LTLMAIN:=./src/temporal_ltl/temporal_ltl_prove.native

WHL2_TEST_FILES:=$(shell find ./benchmarks/procs -name '*.wl2' | sort)
CTL_TEST_FILES:=$(shell find ./benchmarks/temporal -name '*.ctl' | sort)

export

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

sl-tests:
	$(MAKE) -C $(BENCHDIR)/sl all

sl-%-tests:
	$(MAKE) -C $(BENCHDIR)/sl $*

whl-tests:
	-@for TST in $(BENCHDIR)/whl/*.wl ; do echo $$TST: ; _build/$(PR2MAIN) $(TST_OPTS) -P $$TST ; echo ; done

whl_abd-tests:
	-@for TST in $(BENCHDIR)/whl_abd/*.wl ; do echo $$TST ; _build/$(ABD2MAIN) $(TST_OPTS) -P $$TST ; echo ; done

proc-tests:
	-@for TST in $(WHL2_TEST_FILES) ; do echo $$TST: ; _build/$(XTDPRMAIN) $(TST_OPTS) -all -P $$TST ; echo ; done

ctl-tests:
	-@for TST in $(CTL_TEST_FILES) ; do echo $$TST: ; _build/$(CTLMAIN) $(TST_OPTS) -P $$TST ; echo ; done

ltl-tests:
	-@for TST in $(BENCHDIR)/temporal/ltl/*.ltl ; do echo $$TST: ; _build/$(LTLMAIN) $(TST_OPTS) -P $$TST ; echo ; done

aplas-tests: sl-tests #goto-tests #fo-tests

tp-tests: sl-tests whl-tests xsf-tests #fo-tests

abd-tests: whl_abd-tests

all-tests: tp-tests abd-tests
