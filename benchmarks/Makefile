TARGETS := $(filter-out Makefile,$(wildcard *))
.PHONY: $(TARGETS)

sl-%:
	$(MAKE) -C sl $*

$(TARGETS): %:
	$(MAKE) -C $@ all
