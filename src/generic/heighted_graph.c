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
        if( n > max_node ) max_node = n;
        HNode.insert(n);
        HeightsOf.insert(Pair<int,Int_SET*>(n, new Int_SET()));
    }
}

void Heighted_graph::add_height(int n, int h) {
    if (HeightsOf.count(n) == 0) {
        add_node(n);
    }
    (HeightsOf.at(n))->insert(h);
}

void Heighted_graph::add_edge(int source, int sink) {
    if ( h_change_[source][sink] == 0 ) {
        number_of_edges++;
        h_change_[source][sink] = new Sloped_relation(new Map<int,Int_pair_SET*>(),new Map<int,Int_pair_SET*>(),new Map<Int_pair,int>());
    }
}

int Heighted_graph::get_node_size(void){
    return max_node+1;
}

void Heighted_graph::set_node_size(int node_size){
    max_node = node_size;
}

void Heighted_graph::add_hchange(int source, int source_h, int sink, int sink_h, slope s) {
    add_edge(source, sink);
    h_change_[source][sink]->add(source_h, sink_h, s);
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
    return number_of_edges;
}

void Heighted_graph::init_h_change(void){
    h_change_ = (Sloped_relation***)malloc( sizeof(Sloped_relation**) * (max_node + 1));
    for( int i = 0 ; i < (max_node + 1) ; i++ ){
        h_change_[i] = (Sloped_relation**)malloc( sizeof(Sloped_relation*) * (max_node + 1));
        for( int j = 0 ; j < (max_node + 1) ; j++ ){
            h_change_[i][j] = 0;
        }
    }
}

void Heighted_graph::init_Ccl(void){
    Ccl = (Sloped_Relation_SET***)malloc( sizeof(Sloped_Relation_SET**) * (max_node + 1) );
    for( int i = 0 ; i < (max_node + 1) ; i++ ){
        Ccl[i] = (Sloped_Relation_SET**)malloc( sizeof(Sloped_Relation_SET*) * (max_node + 1) );
        for( int j = 0 ; j < (max_node + 1) ; j++ ){
            Ccl[i][j] = 0;
        }
    }
    for( int source = 0 ; source < (max_node + 1) ; source++ ){
    for( int sink = 0 ; sink < (max_node + 1)  ; sink++ ){
        if( h_change_[source][sink] == 0 ){
            Ccl[source][sink] = new Sloped_Relation_SET();
        }
        else{
            Sloped_Relation_SET* Ccl_e = new Sloped_Relation_SET();
            Ccl_e->insert(h_change_[source][sink]);
            Ccl[source][sink] = Ccl_e;
        }
    }
    }
}

void Heighted_graph::compute_Ccl(void){
    init_Ccl();
    bool done = false;
    while( !done ){
        done = true;
        for( int source = 0 ; source < (max_node + 1) ; source++ ){
        for( int middle = 0 ; middle < (max_node + 1)  ; middle++ ){
        for( int sink = 0 ; sink < (max_node + 1) ; sink++ ){
            for( Sloped_relation* P : *Ccl[source][middle] ){
                if( P->size() == 0 ) continue;
                for( Sloped_relation* Q : *Ccl[middle][sink] ){
                    if( Q->size() == 0 ) continue;
                    Sloped_relation* R = P->compose(Q);
                    if( R->size() == 0 ) continue;
                    if( (Ccl[source][sink])->size() == 0 ){
                        done = false;
                        (Ccl[source][sink])->insert(R);
                    }
                    else{
                        done = true;
                        bool found = false;
                        for( Sloped_relation* S : *Ccl[source][sink] ){
                            if( *S == *R ){
                                found = true;
                                delete R;
                                break;
                            }
                        }
                        if( !found ){
                            done = false;
                            (Ccl[source][sink])->insert(R);
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
    bool found_loop = false;
    for( int node : HNode ){
        Sloped_Relation_SET* Ccl_nd = Ccl[node][node];
        for( Sloped_relation* P : *Ccl_nd ){
            if( P->size() == 0 ) continue;
            Sloped_relation R = P->compute_transitive_closure();
            if( R.size() == 0 ) continue;
            for( int h : *(HeightsOf.at(node)) ){
                Map<Int_pair,int>* slopes = R.get_slopes();
                auto exists = slopes->find(Int_pair(h,h));
                if( exists == slopes->end() ) continue;
                if( exists->second == Downward ){
                    found_loop = true;
                    break;
                }
            }
            if( !found_loop ) {
                return false;
            }
        }
    }
    return true;
}

void Heighted_graph::clean(void){
    for( int source = 0 ; source < (max_node + 1) ; source++ ){
        for( int sink = 0 ; sink < (max_node + 1) ; sink++ ){
            for( Sloped_relation* s : *(Ccl[source][sink]) ){
                delete s;
            }
            delete Ccl[source][sink];
        }
        delete h_change_[source];
        delete Ccl[source];
    }
    delete Ccl;
    delete h_change_;
    for( auto p : HeightsOf){
        delete p.second;
    }
}

void Heighted_graph::print_Ccl(void){
    for( int source = 0 ; source < (max_node + 1) ; source++ ){
    for( int sink = 0 ; sink < (max_node + 1)  ; sink++ ){
        for( auto S : *Ccl[source][sink]){
            std::cout << "<><><><><><><><><><><><><><><><><><><><><><><><><><><><><>" << std::endl;
            std::cout << source << " " << sink << std::endl;
            S->print_();
            std::cout << "<><><><><><><><><><><><><><><><><><><><><><><><><><><><><>" << std::endl;
        }
    }
    }
}