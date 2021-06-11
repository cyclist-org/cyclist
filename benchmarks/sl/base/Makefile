TESTS:= $(sort $(wildcard *.tst))
.PHONY: all $(TESTS)
all: $(TESTS)

$(TESTS): %.tst:
	@echo "$@"
	-@while read -r SEQ; do \
		echo -n "\t"; \
		(cd "$(ROOT)" && dune exec $(CMD) -- $(TST_OPTS) -S "$$SEQ" || true) ;\
	done < "$@"
