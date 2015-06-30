SHELL=/bin/bash

HARVEST_PATH=../../utils
HARVEST:=$(HARVEST_PATH)/harvest_heap.py

PYTHONPATH:=$(HARVEST_PATH)
export PYTHONPATH

OCAMLRUNPARAM=b
export OCAMLRUNPARAM

MODELS_PATH=./models
STATS_PATH=./stats

STATS_EXTRACTOR=./extract_times.py

EXAMPLES_BIN_DIR=./programs

MC=../../_build/src/seplog/sl_modelcheck.native

TIME=time

SEED=2758437598

.PHONY: models tests stats

models:
	-@BPS=$$(cat $(BP_FILE)); (set -x; $(HARVEST) "$(EXAMPLES_BIN_DIR)/$(EXE)" $$BPS) 
	
tests:
	-@if stat -t $(TEST_FILE_BASE)*.tst >/dev/null 2>&1 ; then \
		for tstfile in $(TEST_FILE_BASE)*.tst; do \
			echo "Reading test file: $$tstfile" ; \
			while read test formula; do \
				echo "running test series: $(MDL_FILE_BASE)-$$test" ; \
				if stat -t $(MODELS_PATH)/$(MDL_FILE_BASE)-$$test*.mdl >/dev/null 2>&1 ; then \
					(for f in $(MODELS_PATH)/$(MDL_FILE_BASE)-$$test*.mdl; do \
						echo "$$f"; \
						(MDL=$$(cat $$f); set -x; $(TIME) $(MC) -D ./$(DEFS_FILE) -F "$$formula" -M "$$MDL") ; \
					done) ; \
				else \
					echo "No files found" ; \
				fi \
			done < $$tstfile ; \
		done ; \
	else \
		echo "No test files found!" ; \
	fi
	
stats:
	-@if stat -t $(TEST_FILE_BASE)*.tst >/dev/null 2>&1 ; then \
		for tstfile in $(TEST_FILE_BASE)*.tst; do \
			STAT_FILE="$(STATS_PATH)/$${tstfile%.*}-$$(date +%N).csv" ; \
			echo "Outputting $$STAT_FILE" ; \
			(TEST_FILE_BASE="$${tstfile%.*}" $(MAKE) tests 2>&1 | $(STATS_EXTRACTOR) > "$$STAT_FILE") ; \
		done ; \
	else \
		echo "No test files found!" ; \
	fi

aplas-stack-models:
	EXE="aplas-stack 100" BP_FILE=aplas-stack.bps $(MAKE) --no-print-directory models 

aplas-stack-tests:
	TEST_FILE_BASE=aplas-stack DEFS_FILE=aplas-stack.defs MDL_FILE_BASE=aplas-stack $(MAKE) --no-print-directory tests 

aplas-stack-stats:
	-@TEST_FILE_BASE=aplas-stack-[0-9] DEFS_FILE=aplas-stack.defs MDL_FILE_BASE=aplas-stack $(MAKE) --no-print-directory stats

clean-aplas-stack:
	rm -f $(MODELS_PATH)/aplas-stack-*.mdl

composite-models:
	EXE="composite 20 $(SEED)" BP_FILE=composite.bps $(MAKE) --no-print-directory models 

composite-tests:
	TEST_FILE_BASE=composite DEFS_FILE=composite.defs MDL_FILE_BASE=composite $(MAKE) --no-print-directory tests 

composite-stats:
	-@TEST_FILE_BASE=composite-[0-9] DEFS_FILE=composite.defs MDL_FILE_BASE=composite $(MAKE) --no-print-directory stats

clean-composite:
	rm -f $(MODELS_PATH)/composite-*.mdl

iter-models:
	EXE="iter 20" BP_FILE=iter.bps $(MAKE) --no-print-directory models 

iter-tests:
	TEST_FILE_BASE=iter DEFS_FILE=iter.defs MDL_FILE_BASE=iter $(MAKE) --no-print-directory tests 

iter-stats:
	-@TEST_FILE_BASE=iter-[0-9] DEFS_FILE=iter.defs MDL_FILE_BASE=iter $(MAKE) --no-print-directory stats

clean-iter:
	rm -f $(MODELS_PATH)/iter-*.mdl

lcset-models:
	EXE="lcset 8 $(SEED)" BP_FILE=lcset.bps $(MAKE) --no-print-directory models 

lcset-tests:
	TEST_FILE_BASE=lcset DEFS_FILE=lcset.defs MDL_FILE_BASE=lcset $(MAKE) --no-print-directory tests 

lcset-stats:
	-@TEST_FILE_BASE=lcset-[0-9] DEFS_FILE=lcset.defs MDL_FILE_BASE=lcset $(MAKE) --no-print-directory stats

clean-lcset:
	rm -f $(MODELS_PATH)/lcset-*.mdl
	
queue-models:
	EXE="queue 20" BP_FILE=queue.bps $(MAKE) --no-print-directory models ; \
	EXE="queue-dispose 20" BP_FILE=queue-dispose.bps $(MAKE) --no-print-directory models

queue-tests:
	TEST_FILE_BASE=queue DEFS_FILE=queue.defs MDL_FILE_BASE=queue $(MAKE) --no-print-directory tests 

queue-stats:
	-@TEST_FILE_BASE=queue-[0-9] DEFS_FILE=queue.defs MDL_FILE_BASE=queue $(MAKE) --no-print-directory stats

clean-queue:
	rm -f $(MODELS_PATH)/queue-*.mdl

schorr-waite-models:
	-@for i in `seq 1 20`; do \
		EXE="schorr_waite $$i $(SEED)" BP_FILE=schorr-waite.bps $(MAKE) --no-print-directory models ; \
	done 

schorr-waite-tests:
	TEST_FILE_BASE=schorr-waite DEFS_FILE=schorr-waite.defs MDL_FILE_BASE=schorr-waite $(MAKE) --no-print-directory tests 

schorr-waite-stats:
	-@TEST_FILE_BASE=schorr-waite-[ip] DEFS_FILE=schorr-waite.defs MDL_FILE_BASE=schorr-waite $(MAKE) --no-print-directory stats

clean-schorr-waite:
	rm -f $(MODELS_PATH)/schorr-waite-*.mdl

spatial-true-stats:
	-@(TEST_FILE_BASE=aplas-stack-spTrue DEFS_FILE=aplas-stack.defs MDL_FILE_BASE=aplas-stack $(MAKE) --no-print-directory tests 2>&1 ; \
	TEST_FILE_BASE=composite-spTrue DEFS_FILE=composite.defs MDL_FILE_BASE=composite $(MAKE) --no-print-directory tests 2>&1 ; \
	TEST_FILE_BASE=aplas-stack-spTrue DEFS_FILE=aplas-stack.defs MDL_FILE_BASE=aplas-stack $(MAKE) --no-print-directory tests 2>&1 ; \
	TEST_FILE_BASE=aplas-stack-spTrue DEFS_FILE=aplas-stack.defs MDL_FILE_BASE=aplas-stack $(MAKE) --no-print-directory tests 2>&1 ; \
	TEST_FILE_BASE=aplas-stack-spTrue DEFS_FILE=aplas-stack.defs MDL_FILE_BASE=aplas-stack $(MAKE) --no-print-directory tests 2>&1 ; \
	TEST_FILE_BASE=aplas-stack-spTrue DEFS_FILE=aplas-stack.defs MDL_FILE_BASE=aplas-stack $(MAKE) --no-print-directory tests 2>&1 ) \
	| $(STATS_EXTRACTOR) > "$(STATS_PATH)/spTrue-$$(date +%N).csv"
	
	
binary-counter-tests:
	-@for i in `seq 1 5`; do (set -x; $(TIME) $(MC) -D succ-rec0$$i.defs -F "P()" -M "([], [])"); done
	
binary-counter-stats:
	-@($(MAKE) --no-print-directory binary-counter-tests 2>&1) | "$(STATS_PATH)/binary-counter-$$(date +%N).csv"
	
all-models: aplas-stack-models composite-models iter-models lcset-models schorr-waite-models

all-tests: aplas-stack-tests composite-tests iter-tests lcset-tests queue-tests schorr-waite-tests spatial-true-test binary-counter-tests

all-stats: aplas-stack-stats composite-stats iter-stats lcset-stats queue-stats schorr-waite-stats spatial-true-stats binary-counter-stats

clean-stats:
	rm -f $(STATS_PATH)/*.csv

clean-models: clean-aplas-stack clean-composite clean-iter clean-lcset clean-queue clean-schorr-waite
	