#ifndef GENERIC_GRAPH_HH_
#define GENERIC_GRAPH_HH_

#include "types.c"
#include <iostream>
#include <ostream>
#include "graph_algorithms.hpp"

// template <typename Type>
class generic_graph{

private:
    
    // generic graph fields
    Vec<Int_pair>* mapped_edges;
    int max_node = 0;
    graph_algorithms algorithms;



public:



    generic_graph(){}

    ~generic_graph(){}


    // Initialization of the generic graph

    void set_edges(Vec<Int_pair>* edges, int max_node_){
        mapped_edges = edges;
        max_node = max_node_;
    }



    // Algorithms for the generic graph
    Set<Int_pair_SET>* get_SCSs(){ 
        algorithms.get_SCSs(mapped_edges,max_node);
        return algorithms.clone_SCSs();
    }

    void get_ecycles(){ 
        algorithms.get_ECycles(mapped_edges,max_node);
    }

    bool check_SD(SD_decrease_type SD_DEC_TYPE,std::vector<Int_SET*>* HeightsOf,Sloped_relation*** h_change_){ 
        return algorithms.check_SD(SD_DEC_TYPE,HeightsOf,h_change_,mapped_edges,max_node); 
    }
    void CCG(){
        algorithms.get_CCG(new Set<Int_pair>(mapped_edges->begin(), mapped_edges->end()),max_node);
    }
    bool check_XSD(Vec<Int_SET*>* HeightsOf, Sloped_relation*** h_change_,int max_h){
        // Set<Int_pair_SET>* SCSs = get_SCSs();
        return algorithms.check_XSD(max_node,HeightsOf, h_change_,max_h,mapped_edges);
    }


    // Misc.
    void print_edges(){
        for( auto edge : *mapped_edges){
            std::cout << "<" << edge.first << "," << edge.second << "> ";
        }
        std::cout << std::endl;
    }

};







#endif
