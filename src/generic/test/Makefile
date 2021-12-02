TEST_EXE=./soundness

TIMEOUT := $(if $(TIMEOUT),$(TIMEOUT),2)

define test_success
	echo "TEST: $(1), FLAGS=$$FLAGS"
	(timeout $(TIMEOUT) $(TEST_EXE) $(1); EXIT_CODE=$$?; if [ $$EXIT_CODE -eq 0 ]; then echo "TEST PASSED"; elif [ $$EXIT_CODE -eq 124 ]; then echo "TIMEOUT"; else echo "TEST NOT PASSED"; fi; exit 0)
endef

define test_failure
	echo "TEST: $(1), FLAGS=$$FLAGS"
	(timeout $(TIMEOUT) $(TEST_EXE) $(1); EXIT_CODE=$$?; if [ ! $$EXIT_CODE -eq 0 ] && [ ! $$EXIT_CODE -eq 124 ]; then echo "TEST PASSED"; elif [ $$EXIT_CODE -eq 124 ]; then echo "TIMEOUT"; else echo "TEST NOT PASSED"; fi; exit 0)
endef

all: soundness tests

soundness: main.o sloped_relation.o heighted_graph.o
	g++ -g -o soundness main.o sloped_relation.o heighted_graph.o

main.o: main.cpp ../heighted_graph.hpp 
	g++ -I ../ -L/opt/lib -g -c main.cpp -o main.o

heighted_graph.o: ../heighted_graph.hpp ../heighted_graph.c ../sloped_relation.hpp ../types.c
	g++ -I ../ -L/opt/lib -g -c ../heighted_graph.c -o heighted_graph.o

sloped_relation.o: ../sloped_relation.c ../sloped_relation.hpp ../types.c
	g++ -I ../ -L/opt/lib -g -c ../sloped_relation.c -o sloped_relation.o

.PHONY: clean tests test-suite tests-no
.SILENT: tests test-suite test_success test_failure tests-no-opts tests-ff tests-scc tests-min tests-idem tests-ff-scc tests-min-scc tests-ff-idem

clean: 
	rm -f ./*.o soundness

tests: tests-no-opts tests-ff tests-scc tests-min tests-idem tests-ff-scc tests-min-scc tests-ff-idem

tests-no-opts:
	echo "NO OPTIMSATIONS";
	export FLAGS=0; make -s test-suite
tests-ff:
	echo "FAST-FAIL"
	export FLAGS=1; make -s test-suite
tests-scc:
	echo "SCC LOOP CHECK"
	export FLAGS=2; make -s test-suite
tests-min:
	echo "MINIMAL CCL"
	export FLAGS=4; make -s test-suite
tests-idem:
	echo "IDEMPOTENT RELATIONS ONLY"
	export FLAGS=8; make -s test-suite
tests-ff-scc:
	echo "FAST-FAIL, SCC LOOP CHECK"
	export FLAGS=3; make -s test-suite
tests-min-scc:
	echo "MINIMAL CCL, SCC LOOP CHECK"
	export FLAGS=6; make -s test-suite
tests-ff-idem:
	echo "FAST-FAIL, IDEMPOTENT RELATIONS ONLY"
	export FLAGS=9; make -s test-suite

test-suite: soundness
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
