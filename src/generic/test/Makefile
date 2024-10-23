TEST_EXE=./soundness

CFLAGS:=-xc++ -std=c++17

TIMEOUT := $(if $(TIMEOUT),$(TIMEOUT),60)

# Specification for combinations of optimisations used in relational checks
RELATIONAL_OPTIMISATIONS = "" "f" "s" "i" "m" "fm" "fs" "fi" "ms" "fms"

# Specification of node orders used for the order-reduced relations checks
NODE_ORDERS = "0" "1" "2"

define test_success
	echo "TEST: $(1), SPEC=$$SPEC";\
	(timeout $(TIMEOUT) $(TEST_EXE) $(1) $(SPEC); EXIT_CODE=$$?; if [ $$EXIT_CODE -eq 0 ]; then echo "TEST PASSED"; elif [ $$EXIT_CODE -eq 124 ]; then echo "TIMEOUT"; else echo "TEST NOT PASSED"; fi; exit 0)
endef

define test_failure
	echo "TEST: $(1), SPEC=$$SPEC";\
	(timeout $(TIMEOUT) $(TEST_EXE) $(1) $(SPEC); EXIT_CODE=$$?; if [ ! $$EXIT_CODE -eq 0 ] && [ ! $$EXIT_CODE -eq 124 ]; then echo "TEST PASSED"; elif [ $$EXIT_CODE -eq 124 ]; then echo "TIMEOUT"; else echo "TEST NOT PASSED"; fi; exit 0)
endef

all: soundness tests

soundness: main.o sloped_relation.o heighted_graph.o graph.o criterion.soundness.o criterion.process_based.o criterion.flat_cycles.o criterion.descending_unicycles.o criterion.trace_manifold.o directed_graph.o cyclone.o
	g++ -g main.o sloped_relation.o heighted_graph.o graph.o criterion.soundness.o criterion.process_based.o criterion.flat_cycles.o criterion.descending_unicycles.o criterion.trace_manifold.o directed_graph.o cyclone.o `pkg-config --keep-system-libs --libs libspot libbddx` -pthread -o soundness

get_graphs_stats: get_graphs_stats.o sloped_relation.o heighted_graph.o graph.o heighted_graph_parser.o criterion.soundness.o criterion.process_based.o criterion.flat_cycles.o criterion.descending_unicycles.o criterion.trace_manifold.o directed_graph.o cyclone.o
	g++ -g -o get_graphs_stats get_graphs_stats.o sloped_relation.o heighted_graph.o graph.o criterion.soundness.o criterion.process_based.o criterion.flat_cycles.o criterion.descending_unicycles.o criterion.trace_manifold.o directed_graph.o cyclone.o `pkg-config --keep-system-libs --libs libspot libbddx` -pthread

get_graphs_stats.o: get_graphs_stats.cpp ../heighted_graph.hpp ./heighted_graph_parser.cpp ../criterion.descending_unicycles.hpp ../criterion.trace_manifold.hpp
	g++ ${CFLAGS} `pkg-config --cflags libspot libbddx` -I ../ -g -c get_graphs_stats.cpp -o get_graphs_stats.o

run_on_graphs: run_on_graphs.o sloped_relation.o heighted_graph.o graph.o heighted_graph_parser.o criterion.soundness.o criterion.process_based.o directed_graph.o cyclone.o
	g++ -g -o run_on_graphs run_on_graphs.o sloped_relation.o heighted_graph.o graph.o criterion.soundness.o criterion.process_based.o directed_graph.o cyclone.o `pkg-config --libs libspot libbddx`

run_on_graphs.o: run_on_graphs.cpp ../heighted_graph.hpp ./heighted_graph_parser.cpp
	g++ ${CFLAGS} `pkg-config --cflags libspot libbddx` -I ../ -g -c run_on_graphs.cpp -o run_on_graphs.o

test_methods: test_methods.o heighted_graph.o sloped_relation.o criterion.trace_manifold.o criterion.soundness.o
	g++ -g -o test_methods test_methods.o heighted_graph.o sloped_relation.o criterion.trace_manifold.o criterion.soundness.o  `pkg-config --libs libspot libbddx`
test_methods.o: test_methods.cpp ../heighted_graph.hpp ../criterion.trace_manifold.hpp
	g++ ${CFLAGS} `pkg-config --cflags libspot libbddx` -I ../ -g -c test_methods.cpp -o test_methods.o

main.o: main.cpp \
	../heighted_graph.hpp ./heighted_graph_parser.cpp \
	../criterion.flat_cycles.hpp ../criterion.descending_unicycles.hpp ../criterion.trace_manifold.hpp \
	../cyclone.hpp
	g++ ${CFLAGS} `pkg-config --cflags libspot libbddx` -I ../ -g -c main.cpp -o main.o

heighted_graph_parser.o: heighted_graph_parser.cpp ../heighted_graph.hpp ../cyclone.hpp
	g++ ${CFLAGS} `pkg-config --cflags libspot libbddx` -I ../ -g -c heighted_graph_parser.cpp -o heighted_graph_parser.o

heighted_graph.o: ../heighted_graph.hpp ../heighted_graph.c ../sloped_relation.hpp ../types.c
	g++ ${CFLAGS} `pkg-config --cflags libspot libbddx` -I ../ -g -c ../heighted_graph.c -o heighted_graph.o

sloped_relation.o: ../sloped_relation.c ../sloped_relation.hpp ../types.c
	g++ ${CFLAGS} -I ../ -g -c ../sloped_relation.c -o sloped_relation.o

criterion.flat_cycles.o: ../criterion.soundness.c ../heighted_graph.hpp ../criterion.flat_cycles.hpp ../criterion.flat_cycles.c
	g++ ${CFLAGS} -I ../ -c ../criterion.flat_cycles.c -o criterion.flat_cycles.o

criterion.descending_unicycles.o: ../types.c ../criterion.soundness.c ../heighted_graph.hpp ../criterion.descending_unicycles.hpp ../criterion.descending_unicycles.c
	g++ ${CFLAGS} -I ../ -c ../criterion.descending_unicycles.c -o criterion.descending_unicycles.o

criterion.trace_manifold.o: ../criterion.soundness.c ../heighted_graph.hpp ../criterion.trace_manifold.hpp ../criterion.trace_manifold.c
	g++ ${CFLAGS} -I ../ -c ../criterion.trace_manifold.c -o criterion.trace_manifold.o

cyclone.o: ../cyclone.hpp ../cyclone.c ../criterion.soundness.hpp ../criterion.soundness.c \
	../criterion.process_based.hpp ../criterion.process_based.c \
	../criterion.order_reduced.c ../criterion.fwk.c ../criterion.sla.c ../criterion.vla.c \
	../directed_graph.hpp ../directed_graph.c \
	../graph.hpp ../graph.c \
	../criterion.flat_cycles.hpp ../criterion.flat_cycles.c ../criterion.flat_cycles.generalized.c \
	../criterion.descending_unicycles.hpp ../criterion.descending_unicycles.c \
	../criterion.trace_manifold.hpp ../criterion.trace_manifold.c \
	../criterion.sprenger_dam.c ../criterion.no_flat_extended_cycles.c \
	../heighted_graph.hpp ../sloped_relation.hpp ../types.c
	g++ ${CFLAGS} `pkg-config --cflags libspot libbddx` -I ../ -g -c ../cyclone.c -o cyclone.o

criterion.soundness.o: ../criterion.soundness.c ../criterion.soundness.hpp
	g++ ${CFLAGS} -I ../ -g -c ../criterion.soundness.c -o criterion.soundness.o

criterion.process_based.o: ../criterion.process_based.c ../criterion.process_based.hpp ../types.c ../criterion.soundness.hpp ../heighted_graph.hpp
	g++ ${CFLAGS} `pkg-config --cflags libspot libbddx` -I ../ -g -c ../criterion.process_based.c -o criterion.process_based.o

directed_graph.o: ../directed_graph.c ../directed_graph.hpp ../types.c
	g++ ${CFLAGS} -I ../ -g -c ../directed_graph.c -o directed_graph.o

graph.o: ../graph.c ../graph.hpp ../sloped_relation.hpp ../types.c
	g++ ${CFLAGS} -I ../ -g -c ../graph.c -o graph.o

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
    tests-fwk

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
    tests-fwk

clean:
	rm -f ./*.o soundness

run-get-graphs-stats: get_graphs_stats
	./get_graphs_stats $(GRAPHS_DATABASE_PATH) 3
	# ./get_graphs_stats '/Users/matanshaked/M.Sc research/Thesis/cyclist_graphs/cyclist_output_graphs' 3
	# ./get_graphs_stats '/Users/matanshaked/M.Sc research/Thesis/cyclist_graphs/cyclist_output_graphs/sl'
	# ./get_graphs_stats '/Users/matanshaked/M.Sc research/Thesis/cyclist_graphs/cyclist_output_graphs/fo'
	# ./get_graphs_stats '/Users/matanshaked/M.Sc research/Thesis/cyclist_graphs/cyclist_output_graphs/fo/minimized'

	# ./get_graphs_stats ../../../../cyclist_graphs/unminimized/sl
	# ./get_graphs_stats ./loops_tests
	# ./get_graphs_stats ./delete_me_graphs

run-on-graphs-sh-single: run_on_graphs
	./run_on_graphs delete_me_graphs H0fms

run-on-graphs-sh-new: run_on_graphs
	./run_on_graphs ../../../../cyclist_graphs/unminimized/sl H0fms
	# ./run_on_graphs ../../../../cyclist_graphs/unminimized/sl
	# ./run_on_graphs graphs H0fms
run-on-graphs-or-new: run_on_graphs
	# ./run_on_graphs ../../../graphs/unminimized/sl H0fms
	./run_on_graphs graphs O0fms

run-on-single-graph-sh: soundness
	$(TEST_EXE) $(file) H0fms;

tests: tests-automata tests-rel

tests-set: tests-sd tests-xsd
tests-rel: tests-ortl tests-fwk
tests-automata: tests-sla tests-vla

tests-ortl:
	for ORDER in $(NODE_ORDERS); do \
		for OPTIMISATION in $(RELATIONAL_OPTIMISATIONS); do \
			export SPEC="O$$ORDER$$OPTIMISATION"; make -s test-suite-full; \
		done; \
	done

tests-fwk:
	for OPTIMISATION in $(RELATIONAL_OPTIMISATIONS); do \
		export SPEC="F$$OPTIMISATION"; make -s test-suite-full; \
	done; \

tests-sla:
	echo "SLA"
	export SPEC=S; make -s test-suite-full

tests-vla:
	echo "VLA"
	export SPEC=V; make -s test-suite-full

tests-cy:
	echo "CYCLONE"
	export SPEC="H0fms"; make -s test-suite-full; \

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
	$(call test_success, graph_24)
	$(call test_success, graph_25)

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
