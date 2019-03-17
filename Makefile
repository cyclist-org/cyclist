null  :=
space := $(null) #
comma := ,

PKGS := pcre,unix,str,ocamlgraph,dynlink
TAGS := debug,explain,annot,use_libsoundness
OCAMLDOC := ocamldoc -hide-warnings
INCLUDES := $(subst $(space),$(comma),$(strip $(wildcard src/*)))
OCB := ocamlbuild -use-ocamlfind -j 8 -ocamldoc "$(OCAMLDOC)" -pkgs $(PKGS) -tags $(TAGS) -Is $(INCLUDES)
OCAMLFORMAT_EXE := ocamlformat

OUR_DIRS := src/generic src/procedure src/seplog src/slsat src/soundness src/util src/while
OUR_SRCS := $(shell find -f $(OUR_DIRS) ! -name slinit.ml -name '*.ml' -or -name '*.mli')


ROOT := $(shell pwd)
DEFS := $(ROOT)/examples/sl.defs
export ROOT DEFS

TARGETS:= \
	src/generic/checkproof.native \
	src/seplog/sl_prove.native \
	src/seplog/sl_disprove.native \
	src/seplog/sl_modelcheck.native \
	src/slsat/sl_satcheck.native \
	src/slsat/slsat_expgen.native \
	src/while/while_prove.native  \
	src/while/while_abduce.native \
	src/procedure/procedure_prove.native \
	src/seplog/sl.top

TEST_TARGETS := \
	tests/test_slterm_bug1.native \
	tests/test_slterm_bug2.native

.PHONY: all check docs

all: $(TARGETS)

check: $(TEST_TARGETS)
	@for TST in _build/tests/test_*.native ; do $$TST ; done

docs: src/cyclist.docdir/index.html

$(TEST_TARGETS): %.native:
	$(OCB) -no-links $@

%.native:
	$(OCB) $@

%.byte:
	$(OCB) $@

%.html:
	$(OCB) $@

%.top:
	$(OCB) $@

clean:
	$(OCB) -clean

%-tests:
	$(MAKE) -C benchmarks $*

.PHONY: fmt
fmt:
	@parallel $(OCAMLFORMAT_EXE) -i ::: $(OUR_SRCS)
