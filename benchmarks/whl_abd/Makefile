TESTS:= $(sort $(wildcard *.wl))
CMD := cyclist
SUB := while abduce

.PHONY: all $(TESTS)
all: $(TESTS)

$(TESTS): %.wl:
	@echo "$@"
	-@echo -n "\t" ; dune exec $(CMD) -- $(SUB) $(TST_OPTS) -P $@

CASES_FROM := files
CASE_NAME := whl_abd
CASE_ARGS = $(SUB) -P
include ../cases.mk
