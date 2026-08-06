TIMEOUT:=$(or $(TIMEOUT),30)
TESTS:= $(sort $(wildcard *.aslwl))
CMD := "src/asl_while/asl_while_prove.exe"

.PHONY: all $(TESTS)

all: $(TESTS)

$(TESTS): %.aslwl:
	-@TST_NAME=$@; \
	TST_CASE="asl-while-$${TST_NAME%.aslwl}"; \
	echo "test case: $$TST_CASE"; \
	(cd "$(ROOT)" && dune exec $(CMD) -- -t $(TIMEOUT) -P "$(CURDIR)/$@" --id="$$TST_CASE" $(TST_OPTS) || echo "Not proved (timeout)")
