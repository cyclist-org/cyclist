TESTS:= $(sort $(wildcard *.prf))
CMD := "src/generic/checkproof.exe"

.PHONY: all $(TESTS)

all: $(TESTS)

# The .prf files hold abstract proof graphs in the edge-list representation,
# which is not checkproof's default; hence -R edge. Use TST_OPTS to pick a
# particular check, e.g. TST_OPTS="--inf-desc vla".
$(TESTS): %.prf:
	-@TST_CASE="soundness-$*"; \
	echo "test case: $$TST_CASE"; \
	(cd "$(ROOT)" && dune exec $(CMD) -- -R edge $(TST_OPTS) -f "$(CURDIR)/$@")
