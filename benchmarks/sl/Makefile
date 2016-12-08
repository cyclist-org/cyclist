TARGETS:= base atva-2014 songbird
.PHONY: all $(TARGETS)

all: $(TARGETS)

$(TARGETS): %:
		@echo "Called with $@"
		$(MAKE) -C $@
