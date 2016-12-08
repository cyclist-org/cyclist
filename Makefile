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

XTDPRMAIN:=./src/procedure/procedure_prove.native
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

%-tests:
	$(MAKE) -C $(BENCHDIR) $*
