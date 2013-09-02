LFLAGS := -lflags ../c_cyclist/cyclist.a,-I,/usr/lib/ocaml/melt 
CFLAGS := -cflags -I,/usr/lib/ocaml/melt
LIBS := -libs str,latex
OCB := ocamlbuild -ocamlopt "ocamlopt.opt -S" -ocamlmktop "ocamlmktop -custom" $(LIBS) $(CFLAGS) $(LFLAGS) -j 8

TARBALL:=cyclist.tar.gz
TMPDIR:=$(shell mktemp -d -u)
ORIGDIR:=$(PWD)
CYCDIR:=$(TMPDIR)/cyclist

TLMAIN:=./tlmain.native
FOMAIN:=./fomain.native
SLMAIN:=./slmain.native
PRMAIN:=./prmain.native
ABDMAIN:=./abdmain.native
ABD2MAIN:=./abd2main.native
CCMAIN:=./ccmain.native
EXPGENMAIN:=./expgen.native

all:
	$(OCB) $(FOMAIN) $(SLMAIN) $(PRMAIN) $(ABDMAIN) $(ABD2MAIN) $(CCMAIN) $(EXPGENMAIN) sl.top

%.native: 
	$(OCB) "$@"

clean:
	$(OCB) -clean

fo-tests:
	-@for TST in tests/fo/*.tst ; do $(FOMAIN) $(TST_OPTS) -S "`cat $$TST`" ; done

sl-tests:
	-@for TST in tests/sl/*.tst ; do $(SLMAIN) $(TST_OPTS) -S "`cat $$TST`" ; done

pr-tests:
	-@for TST in tests/pr/*.tc ; do $(PRMAIN) $(TST_OPTS) -P $$TST ; done

abd-tests: 
	-@for TST in tests/abd/*.tc ; do ulimit -v 1048576 ; echo $$TST: ; $(ABDMAIN) $(TST_OPTS) -P $$TST ; echo ; done

mutant-tests:
	-@for TST in tests/mutant/*.wl ; do echo $$TST: ; $(ABD2MAIN) $(TST_OPTS) -P $$TST ; echo ; done
  
whl-tests:
	-@for TST in tests/whl_abd/*.wl ; do echo $$TST ; $(ABD2MAIN) $(TST_OPTS) -P $$TST ; echo ; done

all-tests: fo-tests sl-tests pr-tests whl-tests mutant-tests

tarball: 
	@rm -f $(TARBALL)
	@mkdir -p $(CYCDIR)
	@cp -a * $(CYCDIR)
	@cd $(TMPDIR) ; tar zcf $(ORIGDIR)/$(TARBALL) cyclist
	@rm -rf $(TMPDIR)
