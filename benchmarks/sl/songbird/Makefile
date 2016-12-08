TESTS:= $(sort $(wildcard *.tst))
DEFS := $(ROOT)/examples/songbird.defs
CMD := $(ROOT)/_build/$(SLMAIN)

.PHONY: all $(TESTS)
all: $(TESTS)

$(TESTS): %.tst:
	@echo "$@"
	-@while read -r SEQ; do \
		echo -n "\t"; \
		$(CMD) -D $(DEFS) $(TST_OPTS) -S "$$SEQ" || true ;\
	done < "$@"
