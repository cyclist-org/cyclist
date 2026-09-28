CMD := "src/cli/cyclist.exe"
SUB := sl prove
export CMD SUB

AVG_CASE:=base songbird atva-2014
.PHONY: all $(AVG_CASE) worstcase cases
all: $(AVG_CASE)

$(AVG_CASE): %:
		$(MAKE) -w -C $@

cases:
	@for d in $(AVG_CASE); do $(MAKE) -s --no-print-directory -C $$d cases || exit 1; done
