TEST_EXE=./soundness

all: soundness

soundness: main.o sloped_relation.o heighted_graph.o
	g++ -g -o soundness main.o sloped_relation.o heighted_graph.o

main.o: main.cpp ../heighted_graph.hpp 
	g++ -I ../ -L/opt/lib -g -c main.cpp -o main.o

heighted_graph.o: ../heighted_graph.hpp ../heighted_graph.c ../sloped_relation.hpp ../types.c
	g++ -I ../ -L/opt/lib -g -c ../heighted_graph.c -o heighted_graph.o

sloped_relation.o: ../sloped_relation.c ../sloped_relation.hpp ../types.c
	g++ -I ../ -L/opt/lib -g -c ../sloped_relation.c -o sloped_relation.o

.PHONY: clean tests
.SILENT: tests

clean: 
	rm -f ./*.o soundness

tests: soundness
	TEST="graph_1"; echo "TEST: $$TEST"; ($(TEST_EXE) $$TEST; if [   $$? -eq 0 ]; then echo "TEST PASSED"; else echo "TEST FAILED"; fi; exit 0)
	TEST="graph_2"; echo "TEST: $$TEST"; ($(TEST_EXE) $$TEST; if [ ! $$? -eq 0 ]; then echo "TEST PASSED"; else echo "TEST FAILED"; fi; exit 0)
	TEST="graph_3"; echo "TEST: $$TEST"; ($(TEST_EXE) $$TEST; if [   $$? -eq 0 ]; then echo "TEST PASSED"; else echo "TEST FAILED"; fi; exit 0)
	TEST="graph_4"; echo "TEST: $$TEST"; ($(TEST_EXE) $$TEST; if [   $$? -eq 0 ]; then echo "TEST PASSED"; else echo "TEST FAILED"; fi; exit 0)
	TEST="graph_5"; echo "TEST: $$TEST"; ($(TEST_EXE) $$TEST; if [   $$? -eq 0 ]; then echo "TEST PASSED"; else echo "TEST FAILED"; fi; exit 0)
	TEST="graph_6"; echo "TEST: $$TEST"; ($(TEST_EXE) $$TEST; if [ ! $$? -eq 0 ]; then echo "TEST PASSED"; else echo "TEST FAILED"; fi; exit 0)
	TEST="graph_7"; echo "TEST: $$TEST"; ($(TEST_EXE) $$TEST; if [ ! $$? -eq 0 ]; then echo "TEST PASSED"; else echo "TEST FAILED"; fi; exit 0)
	TEST="graph_8"; echo "TEST: $$TEST"; ($(TEST_EXE) $$TEST; if [ ! $$? -eq 0 ]; then echo "TEST PASSED"; else echo "TEST FAILED"; fi; exit 0)
	TEST="graph_8a"; echo "TEST: $$TEST"; ($(TEST_EXE) $$TEST; if [ ! $$? -eq 0 ]; then echo "TEST PASSED"; else echo "TEST FAILED"; fi; exit 0)
	TEST="graph_8b"; echo "TEST: $$TEST"; ($(TEST_EXE) $$TEST; if [   $$? -eq 0 ]; then echo "TEST PASSED"; else echo "TEST FAILED"; fi; exit 0)
