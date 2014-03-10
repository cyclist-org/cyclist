#OCB := ocamlbuild -ocamlopt "ocamlopt.opt -S" -ocamlmktop "ocamlmktop -custom" -j 8 
OCB := ocamlbuild -j 8 

TARBALL:=cyclist.tar.gz
TMPDIR:=$(shell mktemp -d -u)
ORIGDIR:=$(PWD)
CYCDIR:=$(TMPDIR)/cyclist

#TLMAIN:=./src/temporal/tlmain.native
FOMAIN:=./src/firstorder/fo_prove.native
SLMAIN:=./src/seplog/sl_prove.native
PRMAIN:=./src/goto/goto_prove.native
ABDMAIN:=./src/goto/goto_abduce.native
PR2MAIN:=./src/while/while_prove.native
ABD2MAIN:=./src/while/while_abduce.native

all:
	$(OCB) all.otarget

%.native: 
	$(OCB) "$@"

clean:
	$(OCB) -clean

fo-tests:
	-@for TST in tests/fo/*.tst ; do _build/$(FOMAIN) $(TST_OPTS) -S "`cat $$TST`" ; done

sl-tests:
	-@for TST in tests/sl/*.tst ; do _build/$(SLMAIN) $(TST_OPTS) -S "`cat $$TST`" ; done

pr-tests:
	-@for TST in tests/pr/*.tc ; do _build/$(PRMAIN) $(TST_OPTS) -P $$TST ; done

abdgoto-tests: 
	-@for TST in tests/abd/*.tc ; do ulimit -v 1048576 ; echo $$TST: ; _build/$(ABDMAIN) $(TST_OPTS) -P $$TST ; echo ; done

sf-tests:
	-@for TST in tests/sf/*.wl ; do echo $$TST: ; _build/$(PR2MAIN) $(TST_OPTS) -P $$TST ; echo ; done

mutant-tests:
	-@for TST in tests/mutant/*.wl ; do echo $$TST: ; _build/$(ABD2MAIN) $(TST_OPTS) -P $$TST ; echo ; done
  
whl_abd-tests:
	-@for TST in tests/whl_abd/*.wl ; do echo $$TST ; _build/$(ABD2MAIN) $(TST_OPTS) -P $$TST ; echo ; done

tp-tests: fo-tests sl-tests pr-tests sf-tests

abd-tests: abdgoto-tests whl_abd-tests mutant-tests 

all-tests: tp-tests abd-tests

tarball: 
	@rm -f $(TARBALL)
	@mkdir -p $(CYCDIR)
	@cp -a * $(CYCDIR)
	@cd $(TMPDIR) ; tar zcf $(ORIGDIR)/$(TARBALL) cyclist
	@rm -rf $(TMPDIR)
