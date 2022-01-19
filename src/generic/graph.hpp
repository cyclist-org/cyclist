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
    std::list<int>*         adj;
    std::vector<Int_pair>*  edges;
    Map<int,Int_pair_SET>*  SCCs;
    Map<int,int>*           node_idxs;

    std::vector<Int_SET*>*  HeightsOf;
    Sloped_relation***      h_change_;

    Map<int,std::vector<Int_SET*>>* height_families;
    
    int*                    disc;
    int*                    low;
    std::stack<int>*        st;
    bool*                   stackMember;

    void                    mark_nodes(int u);
    void                    get_SCCs (std::vector<Int_pair>*);
    void                    extract_SCC(int u);

    std::vector<std::vector<Int_pair>*>* get_subgraphs(std::vector<Int_pair>*);

public:

    Graph(std::vector<Int_pair>*,std::vector<Int_SET*>*,Sloped_relation***,int,int);
    ~Graph(void);

    bool check_SD(void);
    bool check_descending_SD(std::vector<Int_pair>*);
    bool enumerate_and_check_SD(int,std::vector<int>*,Map<int,int>*,std::vector<int>*);

    void print_SCCs(void);


    void a();

};





#endif