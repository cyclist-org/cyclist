#ifndef HEIGHTED_GRAPH_HH_
#define HEIGHTED_GRAPH_HH_

#include <iostream>
#include <fstream>
#include <set>
#include <utility>
#include "sloped_relation.hpp"
#include "types.c"

class Heighted_graph {
private:
    Int_SET HNode;
    std::set<Int_pair> Edge;
    Map<Int_pair,Sloped_relation> h_change;
    Map<int,Int_SET*> HeightsOf;
    Map<Int_pair,std::set<Sloped_relation>*> Ccl;
public:

    Heighted_graph(void){}

    // Methods for constructing the height graph
    void add_node(int n);
    void add_height(int n, int h);
    void add_edge(int source, int sink);
    void add_hchange(int source, int source_h, int sink, int sink_h, slope s);
    void add_stay(int source_node, int source_h, int sink_node, int sink_h);
    void add_decrease(int source_node, int source_h, int sink_node, int sink_h);
    int num_nodes(void);
    int num_edges(void);
    void compute_Ccl(void);
    bool check_soundness(void);
    void clean(void);
    void print_Ccl(void);
};

#endif