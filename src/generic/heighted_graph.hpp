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
    int                     max_node = -1;
    int                     number_of_edges = 0;
    Int_SET                 HNode;
    Map<int,Int_SET*>       HeightsOf;
    Sloped_relation***      h_change_;
    Sloped_Relation_SET***  Ccl;

    bool check_self_loop(Sloped_relation *R, int node, int opts);
    bool check_Ccl(int opts);

public:

    // Option flags
    static const int FAIL_FAST;
    static const int USE_SCC_CHECK;
    static const int USE_IDEMPOTENCE;
    static const int USE_MINIMALITY;

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
    int get_node_size(void);
    void set_node_size(int node_size);
    void init(void);
    bool check_soundness(int opts);
    void clean(void);
    void print_Ccl(void);
};

#endif