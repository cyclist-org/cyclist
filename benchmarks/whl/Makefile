TESTS:= $(sort $(wildcard *.wl))
CMD := cyclist
SUB := while prove

.PHONY: all $(TESTS)
all: $(TESTS)

$(TESTS): %.wl:
	@echo "$@"
	-@echo -n "\t" ; dune exec $(CMD) -- $(SUB) $(TST_OPTS) -D $(DEFS) -P $@

CASES_FROM := files
CASE_NAME := whl
CASE_ARGS = $(SUB) -D $(call rel,$(DEFS)) -P
include ../cases.mk
