CMD := $(ROOT)/_build/src/seplog/sl_prove.native
export CMD

TARGETS:= base atva-2014 songbird
.PHONY: all $(TARGETS)

all: $(TARGETS)

$(TARGETS): %:
		@echo "Called with $@"
		$(MAKE) -C $@
