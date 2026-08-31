CMD := "src/cli/cyclist.exe"
SUB := sl prove
export CMD SUB

AVG_CASE:=base songbird atva-2014
.PHONY: all $(AVG_CASE) worstcase
all: $(AVG_CASE)

$(AVG_CASE): %:
		$(MAKE) -C $@
