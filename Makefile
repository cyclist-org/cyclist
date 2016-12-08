null  :=
space := $(null) #
comma := ,

PKGS := pcre,melt,unix,str,ocamlgraph,dynlink
TAGS := debug,explain,annot,use_libsoundness
OCAMLDOC := ocamldoc -hide-warnings
INCLUDES := $(subst $(space),$(comma),$(strip $(wildcard src/*)))
OCB := ocamlbuild -use-ocamlfind -j 8 -ocamldoc "$(OCAMLDOC)" -pkgs $(PKGS) -tags $(TAGS) -Is $(INCLUDES)
ROOT := $(shell pwd)
DEFS := $(ROOT)/examples/sl.defs

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
	$(MAKE) -C benchmarks $*
