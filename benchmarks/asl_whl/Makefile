TIMEOUT:=$(or $(TIMEOUT),30)
TESTS:= $(sort $(wildcard *.aslwl))
CMD := "src/cli/cyclist.exe"
SUB := asl-while prove

.PHONY: all $(TESTS)

all: $(TESTS)

$(TESTS): %.aslwl:
	-@TST_NAME=$@; \
	TST_CASE="asl-while-$${TST_NAME%.aslwl}"; \
	echo "test case: $$TST_CASE"; \
	(cd "$(ROOT)" && dune exec $(CMD) -- $(SUB) -t $(TIMEOUT) -P "$(CURDIR)/$@" --id="$$TST_CASE" $(TST_OPTS) || echo "Not proved (timeout)")
