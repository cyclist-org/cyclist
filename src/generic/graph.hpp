#ifndef GRAPH_HH_
#define GRAPH_HH_

#include "types.c"

#include <stack>
#include <vector>
#include <iostream>
#include <ostream>
#include "sloped_relation.hpp"
#define NIL -1

class Graph {

private:
    int                     max_node;
    int                     max_height;
    std::vector<Int_pair>*  edges;
    Map<int,int>*           node_idxs;

    std::set<Int_pair_SET> checked_sccs;

    std::vector<Int_SET*>*  HeightsOf;
    Sloped_relation***      h_change_;

    std::vector<Map<int,int>>***   slope_change_functions;
    
    std::set<std::vector<Pair<Int_pair,Int_pair>>>* ext_graphs;

    void                    mark_nodes(int,std::list<int>*,int*,int*,std::stack<int>*,bool*);
    void                    extract_SCC(int,Map<int,Int_pair_SET>*,std::list<int>*,int*,int*,std::stack<int>*,bool*);
    void                    get_SCCs(std::vector<Int_pair>*,Map<int,Int_pair_SET>*);

    bool                    check_descending_SD(std::vector<Int_pair>*);
    bool                    check_descending_SD_singleton(Map<int,Int_pair_SET>*);
    bool                    enumerate_and_check_SD(int,std::vector<int>*,Map<int,int>*,std::vector<int>*,std::vector<Int_pair>*);
    bool                    check_SCCs_SD_decreasing(SD_decrease_type,std::vector<Int_pair>*,std::vector<std::vector<Int_pair>*>*,Map<int,Int_pair_SET>*);
    bool                    check_SCCs_SD_decreasing(SD_decrease_type,std::vector<std::vector<Int_pair>*>*,Map<int,Int_pair_SET>*);
    bool                    check_SCCs_SD_decreasing(SD_decrease_type,std::vector<Int_pair>*);

    void                    get_functions(std::vector<int>*,std::vector<int>*,std::set<Map<int,int>>*,Int_SET*,Int_pair);
    bool                    check_descending_WeakSD(std::vector<Int_pair>*);
    void                    enumerate_height_families(void);
    void                    init_slope_changing_functions(void);
    void                    get_extended_graphs(int curr,std::vector<int>* idxs,std::vector<Int_pair>* edges);
    bool                    check_set_choice_decrease(std::vector<Pair<Int_pair,Int_pair>>*,std::vector<Int_pair>*);
    std::set<Map<int,int>>* enumerate_slope_changing_functions(Int_pair);


    std::vector<std::vector<Int_pair>*>* get_subgraphs(std::vector<Int_pair>*);

public:

    Graph(std::vector<Int_pair>*,std::vector<Int_SET*>*,Sloped_relation***,int,int);
    ~Graph(void);
    bool check_SD(SD_decrease_type);
    void print_SCCs(Map<int,Int_pair_SET>*);

};





#endif
