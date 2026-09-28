# The `cases` target of a benchmark family, which lists its test cases for
# proof-trace.py: one line per case, holding its name, the directory of the
# family, and then the cyclist arguments that run it, all followed by a tab.
# Paths are relative to $(ROOT).
#
# Include this at the end of the family's Makefile, having set
#   CASES_FROM  lines, if each line of each of $(TESTS) is a case, or files,
#               if each of $(TESTS) is;
#   CASE_NAME   the prefix of the case names, which go on to number the line
#               of the test file, or name the file;
#   CASE_ARGS   the cyclist arguments, the last of which takes the line or
#               the path of the file.

HERE := $(patsubst $(ROOT)/%,%,$(CURDIR))
# $(call rel,PATH) is PATH relative to $(ROOT).
rel = $(patsubst $(ROOT)/%,%,$(1))

.PHONY: cases

ifeq ($(CASES_FROM),lines)
# The test files need not end in a newline.
cases:
	@for TST_NAME in $(TESTS); do \
		TST_COUNT=1; \
		while read -r SEQ || [ -n "$$SEQ" ]; do \
			printf '%s\t' "$(CASE_NAME)-$${TST_NAME%.*}.$$TST_COUNT" "$(HERE)" $(CASE_ARGS) "$$SEQ"; echo; \
			TST_COUNT=$$(($$TST_COUNT+1)); \
		done < "$$TST_NAME"; \
	done
else ifeq ($(CASES_FROM),files)
cases:
	@$(foreach t,$(TESTS),printf '%s\t' "$(CASE_NAME)-$(basename $(notdir $(t)))" "$(HERE)" $(CASE_ARGS) "$(HERE)/$(patsubst ./%,%,$(t))"; echo;)
else
$(error CASES_FROM must be lines or files)
endif
