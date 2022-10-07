TARGETS := $(filter-out Makefile,$(wildcard *))
.PHONY: $(TARGETS)

sl-%:
	$(MAKE) -C sl $*

ctl:
	$(MAKE) -C temporal ctl

ltl:
	$(MAKE) -C temporal ltl

$(TARGETS): %:
	$(MAKE) -C $@ all
