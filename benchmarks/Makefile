TARGETS := $(filter-out Makefile,$(wildcard *))
.PHONY: $(TARGETS)

sl-%:
	$(MAKE) -C sl $*

fo:
	$(MAKE) -C fo all

proc:
	$(MAKE) -C procs all

ctl:
	$(MAKE) -C temporal ctl

ltl:
	$(MAKE) -C temporal ltl

$(TARGETS): %:
	$(MAKE) -C $@ all
