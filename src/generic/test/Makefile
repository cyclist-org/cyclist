TEST_EXE=./soundness

CFLAGS:=-xc++ -std=c++17

TIMEOUT := $(if $(TIMEOUT),$(TIMEOUT),60)

define test_success
	echo "TEST: $(1), SPEC=$$SPEC"
	(timeout $(TIMEOUT) $(TEST_EXE) $(1) $(SPEC); EXIT_CODE=$$?; if [ $$EXIT_CODE -eq 0 ]; then echo "TEST PASSED"; elif [ $$EXIT_CODE -eq 124 ]; then echo "TIMEOUT"; else echo "TEST NOT PASSED"; fi; exit 0)
endef

define test_failure
	echo "TEST: $(1), SPEC=$$SPEC"
	(timeout $(TIMEOUT) $(TEST_EXE) $(1) $(SPEC); EXIT_CODE=$$?; if [ ! $$EXIT_CODE -eq 0 ] && [ ! $$EXIT_CODE -eq 124 ]; then echo "TEST PASSED"; elif [ $$EXIT_CODE -eq 124 ]; then echo "TIMEOUT"; else echo "TEST NOT PASSED"; fi; exit 0)
endef

all: soundness tests

soundness: main.o sloped_relation.o heighted_graph.o  
	g++ -g -o soundness main.o sloped_relation.o heighted_graph.o `pkg-config --libs libspot libbddx`

main.o: main.cpp ../heighted_graph.hpp 
	g++ ${CFLAGS} `pkg-config --cflags libspot libbddx` -I ../ -L/opt/lib -g -c main.cpp -o main.o

heighted_graph.o: ../heighted_graph.hpp ../heighted_graph.c ../sloped_relation.hpp ../types.c
	g++ ${CFLAGS} `pkg-config --cflags libspot libbddx` -I ../ -L/usr/lib -L/opt/lib -g -c ../heighted_graph.c -o heighted_graph.o

sloped_relation.o: ../sloped_relation.c ../sloped_relation.hpp ../types.c
	g++ ${CFLAGS} -I ../ -L/opt/lib -g -c ../sloped_relation.c -o sloped_relation.o

.PHONY: \
    clean \
    tests \
    test-suite-full \
    test-suite-sd \
    test-suite-xsd \
    tests-rel \
    tests-set \
    tests-automata \
    tests-sd \
    tests-xsd \
    tests-sla \
    tests-vla \
    tests-ortl \
    tests-ortl-no-opts \
    tests-ortl-ff \
    tests-ortl-scc \
    tests-ortl-idem \
    tests-ortl-min \
    tests-ortl-ff-min \
    tests-ortl-ff-scc \
    tests-ortl-ff-idem \
    tests-ortl-min-scc \
    tests-ortl-ff-min-scc \
    tests-fwk \
    tests-fwk-no-opts \
    tests-fwk-ff \
    tests-fwk-scc \
    tests-fwk-idem \
    tests-fwk-min \
    tests-fwk-ff-min \
    tests-fwk-ff-scc \
    tests-fwk-ff-idem \
    tests-fwk-min-scc \
    tests-fwk-ff-min-scc

.SILENT: \
    tests \
    test-suite-full \
    test-suite-sd \
    test-suite-xsd \
    tests-rel \
    tests-set \
    tests-automata \
    tests-sd \
    tests-xsd \
    tests-sla \
    tests-vla \
    tests-ortl \
    tests-ortl-no-opts \
    tests-ortl-ff \
    tests-ortl-scc \
    tests-ortl-idem \
    tests-ortl-min \
    tests-ortl-ff-min \
    tests-ortl-ff-scc \
    tests-ortl-ff-idem \
    tests-ortl-min-scc \
    tests-ortl-ff-min-scc \
    tests-fwk \
    tests-fwk-no-opts \
    tests-fwk-ff \
    tests-fwk-scc \
    tests-fwk-idem \
    tests-fwk-min \
    tests-fwk-ff-min \
    tests-fwk-ff-scc \
    tests-fwk-ff-idem \
    tests-fwk-min-scc \
    tests-fwk-ff-min-scc

clean: 
	rm -f ./*.o soundness

tests: tests-automata tests-rel

tests-set: tests-sd tests-xsd
tests-rel: tests-ortl tests-fwk
tests-automata: tests-sla tests-vla

tests-ortl: \
    tests-ortl-no-opts \
    tests-ortl-ff \
    tests-ortl-scc \
    tests-ortl-idem \
    tests-ortl-min \
    tests-ortl-ff-min \
    tests-ortl-ff-scc \
    tests-ortl-ff-idem \
    tests-ortl-min-scc \
    tests-ortl-ff-min-scc

tests-fwk: \
    tests-fwk-no-opts \
    tests-fwk-ff \
    tests-fwk-scc \
    tests-fwk-idem \
    tests-fwk-min \
    tests-fwk-ff-min \
    tests-fwk-ff-scc \
    tests-fwk-ff-idem \
    tests-fwk-min-scc \
    tests-fwk-ff-min-scc

tests-sla:
	echo "SLA"
	export SPEC=S; make -s test-suite-full

tests-vla:
	echo "VLA"
	export SPEC=V; make -s test-suite-full

tests-ortl-no-opts:
	echo "ORTL NO OPTIMSATIONS";
	export SPEC=O; make -s test-suite-full
tests-ortl-ff:
	echo "ORTL FAST-FAIL"
	export SPEC=Of; make -s test-suite-full
tests-ortl-scc:
	echo "ORTL SCC LOOP CHECK"
	export SPEC=Os; make -s test-suite-full
tests-ortl-idem:
	echo "ORTL IDEMPOTENT RELATIONS ONLY"
	export SPEC=Oi; make -s test-suite-full
tests-ortl-min:
	echo "ORTL MINIMAL CCL"
	export SPEC=Om; make -s test-suite-full
tests-ortl-ff-min:
	echo "ORTL FAST-FAIL, MINIMAL CCL"
	export SPEC=Ofm; make -s test-suite-full
tests-ortl-ff-scc:
	echo "ORTL FAST-FAIL, SCC LOOP CHECK"
	export SPEC=Ofs; make -s test-suite-full
tests-ortl-ff-idem:
	echo "ORTL FAST-FAIL, IDEMPOTENT RELATIONS ONLY"
	export SPEC=Ofi; make -s test-suite-full
tests-ortl-min-scc:
	echo "ORTL MINIMAL CCL, SCC LOOP CHECK"
	export SPEC=Oms; make -s test-suite-full
tests-ortl-ff-min-scc:
	echo "ORTL MINIMAL CCL, SCC LOOP CHECK"
	export SPEC=Ofms; make -s test-suite-full

tests-fwk-no-opts:
	echo "FWK NO OPTIMSATIONS";
	export SPEC=F; make -s test-suite-full
tests-fwk-ff:
	echo "FWK FAST-FAIL"
	export SPEC=Ff; make -s test-suite-full
tests-fwk-scc:
	echo "FWK SCC LOOP CHECK"
	export SPEC=Fs; make -s test-suite-full
tests-fwk-idem:
	echo "FWK IDEMPOTENT RELATIONS ONLY"
	export SPEC=Fi; make -s test-suite-full
tests-fwk-min:
	echo "FWK MINIMAL CCL"
	export SPEC=Fm; make -s test-suite-full
tests-fwk-ff-min:
	echo "FWK FAST-FAIL, MINIMAL CCL"
	export SPEC=Ffm; make -s test-suite-full
tests-fwk-ff-scc:
	echo "FWK FAST-FAIL, SCC LOOP CHECK"
	export SPEC=Ffs; make -s test-suite-full
tests-fwk-ff-idem:
	echo "FWK FAST-FAIL, IDEMPOTENT RELATIONS ONLY"
	export SPEC=Ffi; make -s test-suite-full
tests-fwk-min-scc:
	echo "FWK MINIMAL CCL, SCC LOOP CHECK"
	export SPEC=Fms; make -s test-suite-full
tests-fwk-ff-min-scc:
	echo "FWK MINIMAL CCL, SCC LOOP CHECK"
	export SPEC=Ffms; make -s test-suite-full

tests-sd:
	echo "SPRENGER-DAM"
	export SPEC=D; make -s test-suite-sd

tests-xsd:
	echo "EXTENDED SPRENGER-DAM"
	export SPEC=X; make -s test-suite-xsd

test-suite-full: soundness
	$(call test_success, graph_1)
	$(call test_failure, graph_2)
	$(call test_success, graph_3)
	$(call test_success, graph_4)
	$(call test_success, graph_5)
	$(call test_failure, graph_6)
	$(call test_failure, graph_7)
	$(call test_failure, graph_8)
	$(call test_failure, graph_8a)
	$(call test_success, graph_8b)
	$(call test_failure, graph_9)
	$(call test_failure, graph_9a)
	$(call test_failure, graph_9b)
	$(call test_success, graph_10)
	$(call test_failure, graph_11)
	$(call test_failure, graph_12)
	$(call test_success, graph_13)
	$(call test_failure, graph_14)
	$(call test_failure, graph_15)
	$(call test_success, graph_16.00)
	$(call test_success, graph_16.01)
	$(call test_success, graph_16.02)
	$(call test_success, graph_16.03)
	$(call test_success, graph_16.04)
	$(call test_success, graph_16.05)
	$(call test_success, graph_16.06)
	$(call test_success, graph_16.07)
	$(call test_success, graph_16.08)
	$(call test_success, graph_16.09)
	$(call test_success, graph_16.10)
	$(call test_failure, graph_17)
	$(call test_success, graph_18)
	$(call test_success, graph_19)
	$(call test_failure, graph_20a)
	$(call test_failure, graph_20b)
	$(call test_failure, graph_21)
	$(call test_success, graph_22)
	$(call test_success, graph_23)

test-suite-sd: soundness
	$(call test_success, graph_1)
	$(call test_failure, graph_2)
	$(call test_success, graph_3)
	$(call test_success, graph_4)
	$(call test_success, graph_5)
	$(call test_failure, graph_6)
	$(call test_failure, graph_7)
	$(call test_failure, graph_8)
	$(call test_failure, graph_8a)
	$(call test_success, graph_8b)
	$(call test_failure, graph_9)
	$(call test_failure, graph_9a)
	$(call test_failure, graph_9b)
	$(call test_success, graph_10)
	$(call test_failure, graph_11)
	$(call test_failure, graph_12)
	$(call test_failure, graph_13) # succeeds with relational
	$(call test_failure, graph_14)
	$(call test_failure, graph_15)
	$(call test_success, graph_16.00)
	$(call test_success, graph_16.01)
	$(call test_success, graph_16.02)
	$(call test_success, graph_16.03)
	$(call test_success, graph_16.04)
	$(call test_success, graph_16.05)
	$(call test_success, graph_16.06)
	$(call test_success, graph_16.07)
	$(call test_success, graph_16.08)
	$(call test_success, graph_16.09)
	$(call test_success, graph_16.10)
	$(call test_failure, graph_17)
	$(call test_success, graph_18)
	$(call test_failure, graph_19)
	$(call test_failure, graph_20a)
	$(call test_failure, graph_20b)
	$(call test_failure, graph_21)
	$(call test_failure, graph_22)

test-suite-xsd: soundness
	$(call test_success, graph_1)
	$(call test_failure, graph_2)
	$(call test_success, graph_3)
	$(call test_success, graph_4)
	$(call test_success, graph_5)
#	$(call test_failure, graph_6)
	$(call test_failure, graph_7)
	$(call test_failure, graph_8)
	$(call test_failure, graph_8a)
	$(call test_success, graph_8b)
#	$(call test_failure, graph_9)
	$(call test_failure, graph_9a)
	$(call test_failure, graph_9b)
	$(call test_success, graph_10)
	$(call test_failure, graph_11)
	$(call test_failure, graph_12)
	$(call test_success, graph_13)
	$(call test_failure, graph_14)
	$(call test_failure, graph_15)
	$(call test_success, graph_16.00)
	$(call test_success, graph_16.01)
	$(call test_success, graph_16.02)
	$(call test_success, graph_16.03)
	$(call test_success, graph_16.04)
	$(call test_success, graph_16.05)
	$(call test_success, graph_16.06)
	$(call test_success, graph_16.07)
	$(call test_success, graph_16.08)
	$(call test_success, graph_16.09)
	$(call test_success, graph_16.10)
	$(call test_failure, graph_17)
	$(call test_success, graph_18)
	$(call test_failure, graph_19)
	$(call test_failure, graph_20a)
	$(call test_failure, graph_20b)
	$(call test_failure, graph_21)
	$(call test_failure, graph_22)
