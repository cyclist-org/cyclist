#OCB := ocamlbuild -use-ocamlfind -ocamlopt "ocamlopt.opt -S" -ocamlmktop "ocamlmktop -custom" -j 8 
OCB := ocamlbuild -j 8 -ocamlopt "ocamlopt -annot -S"

TARBALL:=cyclist.tar.gz
TMPDIR:=$(shell mktemp -d -u)
ORIGDIR:=$(PWD)
CYCDIR:=$(TMPDIR)/cyclist

FOMAIN:=./src/firstorder/fo_prove.native
SLMAIN:=./src/seplog/sl_prove.native
PRMAIN:=./src/goto/goto_prove.native
PR2MAIN:=./src/while/while_prove.native
XTDPRMAIN:=./src/extended_while/extended_while_prove.native
ABD2MAIN:=./src/while/while_abduce.native
TEMPORALMAIN:=./src/temporal/temporal_prove.native

all:
	$(OCB) all.otarget

%.native: 
	$(OCB) "$@"

%.byte:
	$(OCB) "$@"

clean:
	$(OCB) -clean

#fo-tests:
#	-@for TST in tests/fo/*.tst ; do _build/$(FOMAIN) $(TST_OPTS) -S "`cat $$TST`" ; done

sl-tests:
	-@for TST in tests/sl/*.tst ; do \
		echo "$$TST"; \
		while read -r SEQ; do \
			echo -n "\t"; \
			 _build/$(SLMAIN) $(TST_OPTS) -S "$$SEQ"; \
		done < $$TST; \
	done

sl-atva-tests:
	-@for TST in tests/sl/ATVA-2014/*.tst ; do \
		echo "$$TST"; \
		while read -r SEQ; do \
			echo -n "\t"; \
			 _build/$(SLMAIN) $(TST_OPTS) -D examples/IosifEtAl-ATVA2014.defs -S "$$SEQ"; \
		done < $$TST; \
	done

goto-tests:
	-@for TST in tests/goto/*.tc ; do _build/$(PRMAIN) $(TST_OPTS) -P $$TST ; done

whl-tests:
	-@for TST in tests/whl/*.wl ; do echo $$TST: ; _build/$(PR2MAIN) $(TST_OPTS) -P $$TST ; echo ; done

whl2-tests:
	-@for TST in tests/whl2/*.wl2 ; do echo $$TST: ; _build/$(XTDPRMAIN) $(TST_OPTS) -P $$TST ; echo ; done

whl_abd-tests:
	-@for TST in tests/whl_abd/*.wl ; do echo $$TST ; _build/$(ABD2MAIN) $(TST_OPTS) -P $$TST ; echo ; done

aplas-tests: sl-tests #goto-tests #fo-tests 

tp-tests: sl-tests whl-tests xsf-tests #fo-tests

abd-tests: whl_abd-tests

all-tests: tp-tests abd-tests

tarball: 
	@rm -f $(TARBALL)
	@mkdir -p $(CYCDIR)
	@cp -a * $(CYCDIR)
	@cd $(TMPDIR) ; tar zcf $(ORIGDIR)/$(TARBALL) cyclist
	@rm -rf $(TMPDIR)
