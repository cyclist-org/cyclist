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
						(MDL=$$(cat $$f); set -x; $(TIME) $(MC) $(MC_FLAGS) -D ./$(DEFS_FILE) -F "$$formula" -M "$$MDL") ; \
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
			STAT_FILE="$(STATS_PATH)/$${tstfile%.*}$(STATS_FILE_SUFFIX)-$$(date +%s).csv" ; \
			echo "Outputting $$STAT_FILE" ; \
			(TEST_FILE_BASE="$${tstfile%.*}" $(MAKE) tests 2>&1 | $(STATS_EXTRACTOR) > "$$STAT_FILE") ; \
		done ; \
	else \
		echo "No test files found!" ; \
	fi

aplas-stack-models:
	EXE="aplas-stack 100" BP_FILE=aplas-stack.bps $(MAKE) --no-print-directory models ; \
	EXE="aplas-stack 20" BP_FILE=aplas-stack-spTrue.bps $(MAKE) --no-print-directory models

aplas-stack-tests:
	TEST_FILE_BASE=aplas-stack-[0-9] DEFS_FILE=aplas-stack.defs MDL_FILE_BASE=aplas-stack $(MAKE) --no-print-directory tests 

aplas-stack-ptime-tests:
	-@MC_FLAGS="-CVDET" $(MAKE) aplas-stack-tests --no-print-directory

aplas-stack-stats:
	-@TEST_FILE_BASE=aplas-stack-[0-9] DEFS_FILE=aplas-stack.defs MDL_FILE_BASE=aplas-stack $(MAKE) --no-print-directory stats

aplas-stack-ptime-stats:
	-@MC_FLAGS=-CVDET STATS_FILE_SUFFIX=-cvdet $(MAKE) aplas-stack-stats --no-print-directory

clean-aplas-stack:
	rm -f $(MODELS_PATH)/aplas-stack-*.mdl

composite-models:
	EXE="composite 20 $(SEED)" BP_FILE=composite.bps $(MAKE) --no-print-directory models 

composite-tests:
	TEST_FILE_BASE=composite-[0-9] DEFS_FILE=composite.defs MDL_FILE_BASE=composite $(MAKE) --no-print-directory tests 

composite-stats:
	-@TEST_FILE_BASE=composite-[0-9] DEFS_FILE=composite.defs MDL_FILE_BASE=composite $(MAKE) --no-print-directory stats

clean-composite:
	rm -f $(MODELS_PATH)/composite-*.mdl

iter-models:
	EXE="iter 100" BP_FILE=iter-01.bps $(MAKE) --no-print-directory models ; \
	EXE="iter 20" BP_FILE=iter-02.bps $(MAKE) --no-print-directory models

iter-tests:
	TEST_FILE_BASE=iter-[0-9] DEFS_FILE=iter.defs MDL_FILE_BASE=iter $(MAKE) --no-print-directory tests 

iter-ptime-tests:
	TEST_FILE_BASE=iter-01 DEFS_FILE=iter.defs MDL_FILE_BASE=iter MC_FLAGS=-CVDET $(MAKE) --no-print-directory tests

iter-stats:
	-@TEST_FILE_BASE=iter-[0-9] DEFS_FILE=iter.defs MDL_FILE_BASE=iter $(MAKE) --no-print-directory stats

iter-ptime-stats:
	-@TEST_FILE_BASE=iter-01 DEFS_FILE=iter.defs MDL_FILE_BASE=iter MC_FLAGS=-CVDET STATS_FILE_SUFFIX=-cvdet $(MAKE) --no-print-directory stats

clean-iter:
	rm -f $(MODELS_PATH)/iter-*.mdl

lcset-models:
	EXE="lcset 8 $(SEED)" BP_FILE=lcset.bps $(MAKE) --no-print-directory models 

lcset-tests:
	TEST_FILE_BASE=lcset-[0-9] DEFS_FILE=lcset.defs MDL_FILE_BASE=lcset $(MAKE) --no-print-directory tests 

lcset-stats:
	-@TEST_FILE_BASE=lcset-[0-9] DEFS_FILE=lcset.defs MDL_FILE_BASE=lcset $(MAKE) --no-print-directory stats

clean-lcset:
	rm -f $(MODELS_PATH)/lcset-*.mdl
	
queue-models:
	-@(EXE="queue 100" BP_FILE=queue-01.bps $(MAKE) --no-print-directory models ; \
	EXE="queue-dispose 100" BP_FILE=queue-dispose.bps $(MAKE) --no-print-directory models ; \
	for i in `seq 1 25`; do \
		EXE="queue $$i" BP_FILE=queue-02.bps $(MAKE) --no-print-directory models ; \
	done ; \
	EXE="queue 18" BP_FILE=queue-spTrue-01.bps $(MAKE) --no-print-directory models ; \
	EXE="queue-dispose 18" BP_FILE=queue-spTrue-02.bps $(MAKE) --no-print-directory models ; \
	for i in `seq 1 20`; do \
		EXE="queue $$i" BP_FILE=queue-spTrue-03.bps $(MAKE) --no-print-directory models ; \
	done)

queue-tests:
	TEST_FILE_BASE=queue-[0-9] DEFS_FILE=queue.defs MDL_FILE_BASE=queue MC_FLAGS=-i $(MAKE) --no-print-directory tests 

queue-ptime-tests:
	TEST_FILE_BASE=queue-[0-9] DEFS_FILE=queue.defs MDL_FILE_BASE=queue MC_FLAGS="-CVDET -i" $(MAKE) --no-print-directory tests 

queue-stats:
	-@TEST_FILE_BASE=queue-[0-9] DEFS_FILE=queue.defs MDL_FILE_BASE=queue MC_FLAGS=-i $(MAKE) --no-print-directory stats

queue-ptime-stats:
	TEST_FILE_BASE=queue-[0-9] DEFS_FILE=queue.defs MDL_FILE_BASE=queue MC_FLAGS="-CVDET -i" STATS_FILE_SUFFIX=-cvdet $(MAKE) --no-print-directory stats 

clean-queue:
	rm -f $(MODELS_PATH)/queue-*.mdl

schorr-waite-models:
	-@(for i in `seq 1 100`; do \
		EXE="schorr_waite $$i $(SEED)" BP_FILE=schorr-waite-pre.bps $(MAKE) --no-print-directory models ; \
	done ; \
	for i in `seq 1 30`; do \
		EXE="schorr_waite $$i $(SEED)" BP_FILE=schorr-waite-post.bps $(MAKE) --no-print-directory models ; \
	done ; \
	for i in `seq 1 20`; do \
		EXE="schorr_waite $$i $(SEED)" BP_FILE=schorr-waite-loop.bps $(MAKE) --no-print-directory models ; \
		EXE="schorr_waite $$i $(SEED)" BP_FILE=schorr-waite-spTrue.bps $(MAKE) --no-print-directory models ; \
	done)

schorr-waite-tests:
	TEST_FILE_BASE=schorr-waite-[ip] DEFS_FILE=schorr-waite.defs MDL_FILE_BASE=schorr-waite $(MAKE) --no-print-directory tests 

schorr-waite-ptime-tests:
	TEST_FILE_BASE=schorr-waite-pre DEFS_FILE=schorr-waite.defs MDL_FILE_BASE=schorr-waite MC_FLAGS=-CVDET $(MAKE) --no-print-directory tests 

schorr-waite-stats:
	-@TEST_FILE_BASE=schorr-waite-[ip] DEFS_FILE=schorr-waite.defs MDL_FILE_BASE=schorr-waite $(MAKE) --no-print-directory stats

schorr-waite-ptime-stats:
	TEST_FILE_BASE=schorr-waite-pre DEFS_FILE=schorr-waite.defs MDL_FILE_BASE=schorr-waite MC_FLAGS=-CVDET STATS_FILE_SUFFIX=-cvdet $(MAKE) --no-print-directory stats 

clean-schorr-waite:
	rm -f $(MODELS_PATH)/schorr-waite-*.mdl

spatial-true-tests:
	-@TEST_FILE_BASE=aplas-stack-spTrue DEFS_FILE=aplas-stack.defs MDL_FILE_BASE=aplas-stack $(MAKE) --no-print-directory tests ; \
	TEST_FILE_BASE=composite-spTrue DEFS_FILE=composite.defs MDL_FILE_BASE=composite $(MAKE) --no-print-directory tests ; \
	TEST_FILE_BASE=iter-spTrue DEFS_FILE=iter.defs MDL_FILE_BASE=iter $(MAKE) --no-print-directory tests ; \
	TEST_FILE_BASE=lcset-spTrue DEFS_FILE=lcset.defs MDL_FILE_BASE=lcset $(MAKE) --no-print-directory tests ; \
	TEST_FILE_BASE=queue-spTrue DEFS_FILE=queue.defs MDL_FILE_BASE=queue $(MAKE) --no-print-directory tests ; \
	TEST_FILE_BASE=schorr-waite-spTrue DEFS_FILE=schorr-waite.defs MDL_FILE_BASE=schorr-waite $(MAKE) --no-print-directory tests \

spatial-true-stats:
	-@TEST_FILE_BASE=aplas-stack-spTrue DEFS_FILE=aplas-stack.defs MDL_FILE_BASE=aplas-stack $(MAKE) --no-print-directory stats ; \
	TEST_FILE_BASE=composite-spTrue DEFS_FILE=composite.defs MDL_FILE_BASE=composite $(MAKE) --no-print-directory stats ; \
	TEST_FILE_BASE=iter-spTrue DEFS_FILE=iter.defs MDL_FILE_BASE=iter $(MAKE) --no-print-directory stats ; \
	TEST_FILE_BASE=lcset-spTrue DEFS_FILE=lcset.defs MDL_FILE_BASE=lcset $(MAKE) --no-print-directory stats ; \
	TEST_FILE_BASE=queue-spTrue DEFS_FILE=queue.defs MDL_FILE_BASE=queue $(MAKE) --no-print-directory stats ; \
	TEST_FILE_BASE=schorr-waite-spTrue DEFS_FILE=schorr-waite.defs MDL_FILE_BASE=schorr-waite $(MAKE) --no-print-directory stats \
	
binary-counter-tests:
	-@for i in `seq 1 3`; do (set -x; $(TIME) $(MC) -D succ-rec0$$i.defs -F "P()" -M "([], [])"); done
	
binary-counter-stats:
	-@($(MAKE) --no-print-directory binary-counter-tests 2>&1) | $(STATS_EXTRACTOR) > "$(STATS_PATH)/binary-counter-$$(date +%s).csv"
	
all-models: aplas-stack-models composite-models iter-models lcset-models schorr-waite-models

all-tests: aplas-stack-tests composite-tests iter-tests lcset-tests queue-tests schorr-waite-tests spatial-true-tests binary-counter-tests

all-ptime-tests: aplas-stack-ptime-tests iter-ptime-tests queue-ptime-tests schorr-waite-ptime-tests

all-stats: aplas-stack-stats composite-stats iter-stats lcset-stats queue-stats schorr-waite-stats

all-ptime-stats: aplas-stack-ptime-stats iter-ptime-stats queue-ptime-stats schorr-waite-ptime-stats

quick-tests:
	$(TIME) $(MC) -D queue.defs -F "lseg2(first,middle) * lseg(last,middle)" -M "`cat $(MODELS_PATH)/queue-line_667-0022.mdl`"
	$(TIME) $(MC) -D schorr-waite.defs -F "tree(root,nil)" -M "`cat $(MODELS_PATH)/schorr-waite-pre0026.mdl`"
	$(TIME) $(MC) -D composite.defs -F "tree(node)" -M "`cat $(MODELS_PATH)/composite-tree_add_right0010.mdl`"
	$(TIME) $(MC) -D lcset.defs -F "set_atomic(set)" -M "`cat $(MODELS_PATH)/lcset-add0008.mdl`"
	$(TIME) $(MC) -D succ-rec03.defs -F "P()" -M "([], [])"

clean-stats:
	rm -f $(STATS_PATH)/*.csv

clean-models: clean-aplas-stack clean-composite clean-iter clean-lcset clean-queue clean-schorr-waite
	
