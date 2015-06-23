SHELL=/bin/bash

HARVEST_PATH=../../utils
HARVEST:=$(HARVEST_PATH)/harvest_heap.py

PYTHONPATH:=$(HARVEST_PATH)
export PYTHONPATH

MODELS_PATH=./models

EXAMPLES_BIN_DIR=./programs

MC=../../_build/src/seplog/sl_modelcheck.native

TIME=time

SEED=2758437598

.PHONY: models tests

models:
	-@BPS=$$(cat $(BP_FILE)); (set -x; $(HARVEST) "$(EXAMPLES_BIN_DIR)/$(EXE)" $$BPS) 
	
tests:
	-@while read test formula; do \
		echo "$(MDL_FILE_BASE)-$$test" ; \
		for f in $(MODELS_PATH)/$(MDL_FILE_BASE)-$$test*.mdl; do \
			echo "$$f"; \
			(MDL=$$(cat $$f); set -x; $(TIME) $(MC) -D ./$(DEFS_FILE) -F "$$formula" -M "$$MDL") ; \
		done  \
	done < $(TEST_FILE)

aplas-stack-models:
	EXE=aplas-stack BP_FILE=aplas-stack.bps $(MAKE) --no-print-directory models 

aplas-stack-tests:
	TEST_FILE=aplas-stack.tst DEFS_FILE=aplas-stack.defs MDL_FILE_BASE=aplas-stack $(MAKE) --no-print-directory tests 

clean-aplas-stack:
	rm -f ./models/aplas-stack-*.mdl

composite-models:
	EXE=composite BP_FILE=composite.bps $(MAKE) --no-print-directory models 

composite-tests:
	TEST_FILE=composite.tst DEFS_FILE=composite.defs MDL_FILE_BASE=composite $(MAKE) --no-print-directory tests 

clean-composite:
	rm -f ./models/composite-*.mdl

iter-models:
	EXE=iter BP_FILE=iter.bps $(MAKE) --no-print-directory models 

iter-tests:
	TEST_FILE=iter.tst DEFS_FILE=iter.defs MDL_FILE_BASE=iter $(MAKE) --no-print-directory tests 

clean-iter:
	rm -f ./models/iter-*.mdl

lcset-models:
	EXE=lcset BP_FILE=lcset.bps $(MAKE) --no-print-directory models 

lcset-tests:
	TEST_FILE=lcset.tst DEFS_FILE=lcset.defs MDL_FILE_BASE=lcset $(MAKE) --no-print-directory tests 

clean-lcset:
	rm -f ./models/lcset-*.mdl

schorr-waite-models:
	-@for i in `seq 1 20`; do \
		EXE="schorr_waite $$i $(SEED)" BP_FILE=schorr-waite.bps $(MAKE) --no-print-directory models ; \
	done 

.PHONY: schorr-waite-prepost-test schorr-waite-inv1-test schorr-waite-inv2-test schorr-waite-inv3-test

schorr-waite-prepost-test:
	TEST_FILE=schorr-waite-prepost.tst DEFS_FILE=schorr-waite.defs MDL_FILE_BASE=schorr-waite $(MAKE) --no-print-directory tests 

schorr-waite-inv1-test:
	TEST_FILE=schorr-waite-inv1.tst DEFS_FILE=schorr-waite.defs MDL_FILE_BASE=schorr-waite $(MAKE) --no-print-directory tests 

schorr-waite-inv2-test:
	TEST_FILE=schorr-waite-inv2.tst DEFS_FILE=schorr-waite.defs MDL_FILE_BASE=schorr-waite $(MAKE) --no-print-directory tests 

schorr-waite-inv3-test:
	TEST_FILE=schorr-waite-inv3.tst DEFS_FILE=schorr-waite.defs MDL_FILE_BASE=schorr-waite $(MAKE) --no-print-directory tests 

schorr-waite-tests: schorr-waite-prepost-test schorr-waite-inv1-test schorr-waite-inv2-test schorr-waite-inv3-test

clean-schorr-waite:
	rm -f ./models/schorr-waite-*.mdl

spatial-true-tests:
	./test_sp_true.sh 1 20
	
binary-counter-tests:
	-@for i in `seq 1 5`; do (set -x; $(TIME) $(MC) -D succ-rec0$$i.defs -F "P()" -M "([], [])"); done
	
all-models: aplas-stack-models composite-models iter-models lcset-models

all-tests: aplas-stack-tests composite-tests iter-tests lcset-tests spatial-true-test binary-counter-tests

clean-all: clean-aplas-stack clean-composite clean-iter clean-lcset
	