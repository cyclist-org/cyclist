CMD := "$(ROOT)/_build/src/seplog/sl_prove.native"
export CMD

TARGETS:=$(filter-out Makefile,$(wildcard *))
.PHONY: all $(TARGETS)
all: $(TARGETS)

$(TARGETS): %:
		$(MAKE) -C $@
