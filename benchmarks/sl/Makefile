CMD := "src/seplog/prove.exe"
export CMD

TARGETS:=$(filter-out Makefile,$(wildcard *))
.PHONY: all $(TARGETS)
all: $(TARGETS)

$(TARGETS): %:
		$(MAKE) -C $@
