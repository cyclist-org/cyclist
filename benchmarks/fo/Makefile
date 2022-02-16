CMD := "src/firstorder/prove.exe"
TESTS:= $(sort $(wildcard *.tst))
DEFS := "$(ROOT)/examples/fo.defs"

.PHONY: all $(TESTS)

all: $(TESTS)

$(TESTS): %.tst:
	@echo "$@"
	-@while read -r SEQ; do \
		echo -n "\t"; \
		(cd "$(ROOT)" && dune exec $(CMD) -- -D $(DEFS) $(TST_OPTS) -S "$$SEQ" || true) ;\
	done < "$@"
