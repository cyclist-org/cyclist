#ifndef GRAPH_ALGORITHMS_HH_
#define GRAPH_ALGORITHMS_HH_


#include "types.c"

#include <stack>
#include <vector>
#include <iostream>
#include <ostream>
#include <list>

#include <algorithm>

#include "sloped_relation.hpp"


class graph_algorithms{

private:
    Vec<Pair<int,int>>* edges;
    int max_node_;
    //==========================================
    //USE FOR CYCLES ON THE FLY
    Map<std::vector<Int_pair>,int>* cycle_to_id;
    Map<int,std::vector<Int_pair>>* id_to_cycle;
    int cycle_id = 0;
    
    //==========================================
    // USE: Johnsons Algorithm for finding elementary cycles
    Vec<NodeList>*      AK;
    Vec<NodeList>*      B;
    Vec<bool>*          Blocked;
    Vec<int>*           Stack;
    NodeList*           adj;
    int                 N;
    int                 S;
    //==========================================

    Vec<Int_SET*>*                  HeightsOf;
    Sloped_relation***              h_change_;

    Map<int,Vec<Vec<Int_pair>>>     test;       // this is a map that maps a node to all ecycles that contain this node
    Set<Vec<Int_pair>>              ECycles;    // a set of all ecycles

    Set<Int_pair_SET>               SCSs;

    Set<Int_SET>                    CCG;

    bool ok = false;


    Vec< Pair< Vec<Set<int>> , Vec<Map<int,int>>***> > height_families_and_function_pairs;

    //========================================================================================================================

    //==========================================
    // USE: Johnsons Algorithm for finding elementary cycles [get_ECycles]
    void output();
    void unblock(int U);
    bool circuit(int V);
    //==========================================

    //==========================================
    // USE: CIS ALGO 2018 (https://bmcbioinformatics.biomedcentral.com/track/pdf/10.1186/s12859-019-2837-y.pdf) [get_CCG]
    void enum_CCS(int anchor, int distance, Set<int>* U, Set<int>* C ,int* D , int last_added, Set<int>* prev_U);
    bool is_valid_ext(int s, int v,int* D,int x);
    //==========================================

    bool enumerate_and_check_SD(int curr,std::vector<int>* node_idxs_,Map<int,int>* rev_node_idxs,std::vector<int>* indicies,std::vector<Int_pair>* edges);
    bool check_descending_SD(std::vector<Int_pair>* edges);
    bool check_descending_SD_singleton();


public:
    void get_CCG( Set<Int_pair>* edges,int max_node);
    void get_ECycles(Vec<Int_pair>* edges, int max_node);
    void get_SCSs(Vec<Int_pair>* edges, int max_node);
    bool check_SD(std::vector<Int_SET*>* HeightsOf ,Sloped_relation*** h_change_ ,Vec<Pair<int,int>>* edges_, int max_node_);



    void get_functions_for_edge_wrt_height_family(int max , Vec<int>* ind, Set<int>* h1, Set<int>* h2,Vec<Map<int,int>>* mapp,int u, int v,int curr = 0);
    Vec< Vec<Set<int>>>* get_height_power_sets(int max_h);
    void get_height_families(Vec<Set<int>>* indicies,Vec< Vec<Set<int>>>* height_sets,int curr = 0);
    bool check_XSD(int max_node_,Vec<Int_SET*>* HeightsOf, Sloped_relation*** h_change_, int max_h,Vec<Pair<int,int>>* edges);


    int get_extended_graph(Vec<Set<int>>* height_family,Vec<Map<int,int>>* func_fam,Set<Int_pair>* ext_edges,Map<int,Int_pair>* id_to_ext_edge,Map<Int_pair,int>* ext_edge_to_id);
    bool check_height_family_function_family_decreasing(Vec<Set<int>>* height_family ,Vec<Map<int,int>>* function_family ,Vec<Map<int,int>>*** functions, int curr = 0);

    Set<Int_pair_SET>* clone_SCSs();
};





#endif