TARGETS := $(filter-out Makefile,$(wildcard *))
.PHONY: $(TARGETS)

fo:
	$(MAKE) -C fo all

sl-%:
	$(MAKE) -C sl $*

$(TARGETS): %:
	$(MAKE) -C $@ all
