all: soundness

soundness: main.o sloped_relation.o heighted_graph.o
	g++ -g -o soundness main.o sloped_relation.o heighted_graph.o

main.o: main.cpp ../heighted_graph.hpp 
	g++ -I ../ -L/opt/lib -g -c main.cpp -o main.o

heighted_graph.o: ../heighted_graph.hpp ../heighted_graph.c ../sloped_relation.hpp ../types.c
	g++ -I ../ -L/opt/lib -g -c ../heighted_graph.c -o heighted_graph.o

sloped_relation.o: ../sloped_relation.c ../sloped_relation.hpp ../types.c
	g++ -I ../ -L/opt/lib -g -c ../sloped_relation.c -o sloped_relation.o

.PHONY: clean

clean: 
	rm -f ./*.o soundness