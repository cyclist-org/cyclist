#include "heighted_graph.hpp"
#include "sloped_relation.hpp"
#include "types.c"

#include <iostream>
#include <fstream>
#include <set>
#include <utility>

// Methods for constructing the height graph
void Heighted_graph::add_node(int n) {
    if (HNode.find(n) == HNode.end()) {
        HNode.insert(n);
        HeightsOf.insert(std::pair<int,Int_SET*>(n, new Int_SET()));
    }
}

void Heighted_graph::add_height(int n, int h) {
    if (HeightsOf.count(n) == 0) {
        add_node(n);
    }
    (HeightsOf.at(n))->insert(h);
}

void Heighted_graph::add_edge(int source, int sink) {
    Int_pair edge(source, sink);
    if (Edge.find(edge) == Edge.end()) {
        Edge.insert(edge);
        Sloped_relation s = Sloped_relation();
        h_change.insert(std::pair<Int_pair,Sloped_relation>(edge,s));
    }
}

void Heighted_graph::add_hchange(int source, int source_h, int sink, int sink_h, slope s) {
    add_edge(source, sink);
    Int_pair edge(source, sink);
    h_change.at(edge).add(source_h, sink_h, s);
}

void Heighted_graph::add_stay(int source_node, int source_h, int sink_node, int sink_h) {
    add_hchange(source_node, source_h, sink_node, sink_h, Stay);
}

void Heighted_graph::add_decrease(int source_node, int source_h, int sink_node, int sink_h) {
    add_hchange(source_node, source_h, sink_node, sink_h, Downward);
}

int Heighted_graph::num_nodes(void) {
    return HNode.size();
}

int Heighted_graph::num_edges(void) {
    return Edge.size();
}

void Heighted_graph::compute_Ccl(void){
    int Node_size = HNode.size();
        for( int source = 0 ; source < Node_size ; source++){
        for( int sink = 0 ; sink < Node_size  ; sink++){
            Int_pair p(source,sink);
            auto exists = h_change.find(p);
            if(exists == h_change.end()){
                Ccl.insert(std::pair<Int_pair,std::set<Sloped_relation>*>(p,new std::set<Sloped_relation>()));
            }
            else{
                std::set<Sloped_relation>* Ccl_e = new std::set<Sloped_relation>();
                Ccl_e->insert(h_change.at(p));
                Ccl.insert(std::pair<Int_pair,std::set<Sloped_relation>*>(p,Ccl_e));
            }
        }
        }
    bool done = false;
    while(!done){
        done = true;
        for( int source = 0 ; source < Node_size ; source++){
            for( int middle = 0 ; middle < Node_size  ; middle++){
                for( int sink = 0 ; sink < Node_size ; sink++){
                    Int_pair p1(source,middle);
                    Int_pair p2(middle,sink);
                    Int_pair p3(source,sink);
                    std::set<Sloped_relation>* Ccl_1 = Ccl.at(p1);
                    std::set<Sloped_relation>* Ccl_2 = Ccl.at(p2);
                    std::set<Sloped_relation>* Ccl_3 = Ccl.at(p3);
                    for( Sloped_relation P : *Ccl_1){
                        for( Sloped_relation Q : *Ccl_2){
                            Sloped_relation R = P.compose(&Q);
                            bool found = false;
                            if(Ccl_3->size() == 0 ){
                                done = false;
                                Ccl_3->insert(R);
                            }
                            else{
                                for ( Sloped_relation S : *Ccl_3){
                                    if(R == S){
                                        found = true;
                                        break;
                                    }
                            
                                }
                                if(!found){
                                    done = false;
                                    Ccl_3->insert(R);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

bool Heighted_graph::check_soundness(void){
    Sloped_relation R;
    bool found_loop = false;
    for ( int node : HNode){
        Int_pair p(node,node);
        auto exists = Ccl.find(p);
        if( exists == Ccl.end()){
            continue;
        }
        std::set<Sloped_relation>* Ccl_nd = Ccl.at(p);
        for( Sloped_relation P : *Ccl_nd){
            R = P.compute_transitive_closure();
            for( int h : *(HeightsOf.at(node))){
                Int_pair_SET* slopes = R.get_backward_slopes(h);
                if(slopes == 0) continue;
                for( auto pair : *slopes){
                    if( pair.first == h){
                        if( pair.second == Downward){
                            found_loop = true;
                            break;
                        }
                    }
                }
                if( !found_loop ) {
                    return false;
                }
            }
        }
    }
    return true;
}

void Heighted_graph::clean(void){
    for( auto pair : Ccl){
        delete pair.second;
    }
    for( auto pair : HeightsOf){
        delete pair.second;
    }
}

void Heighted_graph::print_Ccl(void){
    for( const auto& pair : Ccl){
        std::cout << pair.first.first << "," << pair.first.second << std::endl;
        for( Sloped_relation b : *(pair.second)){
            b.print_();
        }
    }
}