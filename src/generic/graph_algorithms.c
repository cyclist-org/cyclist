
#include "graph_algorithms.hpp"
#include "generic_graph.hpp"

#include "types.c"

#include <stack>
#include <vector>
#include <iostream>
#include <ostream>
#include <list>
#include <math.h>

#include <algorithm>

#include "sloped_relation.hpp"

//==========================================
//******************************************
//==========================================
// USE: finding SCS  (CIS ALGO 2018)
bool graph_algorithms::is_valid_ext(int s, int v,int* D,int x){
    if( v < s ) return false;
    if( D[v] > D[x] ) return true;
    return (D[v] == D[x]) && v > x ;
}

void graph_algorithms::enum_CCS(int anchor, int distance,Set<int>* U, Set<int>* C ,int* D , int last_added,Set<int>* prev_U){
    CCG.insert(*U);

    if( *U == *prev_U ) return;
    
    for( auto v : *C ){
        if( is_valid_ext(anchor,v,D,last_added) ){
            Set<int>* C_p = new Set<int>(*C);
            Set<int>* U_p = new Set<int>(*U);
            U_p->insert(v);
            int last_added_p = v;
            int distance_p = distance+1;
            std::list<int>::iterator n;            
            for ( n = adj[v].begin() ; n != adj[v].end() ; ++n){
                C_p->insert(*n);
                if( D[*n] == 0 && *n != anchor) D[*n] = distance_p;
            }
            enum_CCS(anchor,distance_p,U_p,C_p,D,last_added_p,U);
            delete C_p;
            delete U_p;
        }
    }
}

void graph_algorithms::get_CCG( Set<Int_pair>* edges,int max_node) {

    adj = new std::list<int>[max_node]; 
    for( auto e : *edges){
        adj[e.first].push_back(e.second);
    }
    for( int anchor = 0 ; anchor < max_node ; anchor++){
        Set<int>* U = new Set<int>();
        Set<int>* prev_U = new Set<int>();
        Set<int>* C = new Set<int>();
        int D[max_node];
        for( int i = 0 ; i < max_node ; i++){
            D[i] = 0;
        }
        D[anchor] = 0;
        int distance = 1;
        U->insert(anchor);
        int last_added = anchor;
        std::list<int>::iterator n; 
        for ( n = adj[anchor].begin() ; n != adj[anchor].end() ; ++n){
            C->insert(*n);
            D[*n] = distance;
        }
        enum_CCS(anchor,distance,U,C,D,last_added,prev_U);

        delete U;
        delete prev_U;
        delete C;
    }
    delete[] adj;

    // for( auto ccg : CCG ){
    //     for( auto n : ccg ){
    //         std::cout << n << ",";
    //     }
    //     std::cout << std::endl;
    // }
}

//==========================================
//******************************************
//==========================================
// USE: finding ECYCLES Johnsons algo.
void graph_algorithms::output(){
    std::vector<Int_pair> cycle;
    int left = -1;
    std::vector<int> idxs;
    for (auto right = Stack->begin(), E = Stack->end(); right != E; ++right) {
        idxs.push_back(*right);
        if( left != -1 ) cycle.push_back(Int_pair(left-1,(*right)-1));
        left = *right;
    }
    cycle.push_back(Int_pair(left - 1,*(Stack->begin()) - 1));
    for(int node : idxs ){
        auto exists = test.find(node);
        if(exists == test.end()){
            std::vector<std::vector<Int_pair>> vec;
            vec.push_back(cycle);
            test.insert(Pair<int,std::vector<std::vector<Int_pair>>>(node,vec));
        }
        else{
            (exists->second).push_back(cycle);
        }
    }
    cycle_to_id->insert(std::pair(cycle,cycle_id));
    id_to_cycle->insert(std::pair(cycle_id,cycle));
    cycle_id++;
    ECycles.insert(cycle);
}

void graph_algorithms::unblock(int U){
    Blocked->at(U - 1) = false;
    while( !B->at(U - 1).empty() ){
        int W = B->at(U - 1).front();
        B->at(U - 1).pop_front();
        if (Blocked->at(W - 1)) {
            unblock(W);
        }
    }
}

bool graph_algorithms::circuit(int V){
    bool F = false;
    Stack->push_back(V);
    Blocked->at(V - 1) = true;

    for (int W : AK->at(V - 1)) {
        if (W == S) {
            output();
            F = true;
        }
        else if (W > S && !Blocked->at(W - 1)) {
            F = circuit(W);
        }
    }

    if (F) {
        unblock(V);
    }
    else {
        for (int W : AK->at(V - 1)) {
            auto IT = std::find((B->at(W - 1)).begin(), (B->at(W - 1)).end(), V);
            if (IT == (B->at(W - 1)).end()) {
                (B->at(W - 1)).push_back(V);
            }
        }
    }

    Stack->pop_back();
    return F;
}

//==========================================
//******************************************
//==========================================
void graph_algorithms::get_ECycles(Vec<Int_pair>* edges, int max_node){
    N = max_node+1;
    AK = new std::vector<NodeList>(N);
    B = new std::vector<NodeList>(N);
    Stack = new std::vector<int>();
    Blocked = new std::vector<bool>(N);
    for( auto edge : *edges ){
        AK->at(edge.first).push_back(edge.second+1);
    }
    Stack->clear();
    S = 1;

    while (S < N) {
        for (int I = S; I <= N; ++I) {
            Blocked->at(I - 1) = false;
            B->at(I - 1).clear();
        }
        circuit(S);
        ++S;
    }

    delete AK;
    delete B;
    delete Stack;
    delete Blocked;

    // for( auto pair : test ){
    //     std::cout << "ECycles with the node " << pair.first - 1 << ":" << std::endl;
    //     auto cycles = pair.second;
    //     for( auto edges : cycles ){
    //         for(auto e : edges ){
    //             std::cout << "<" << e.first << "," << e.second << ">";
    //         }
    //         std::cout << std::endl << "---------------" << std::endl;;
    //     }
    // }
}

void graph_algorithms::get_SCSs(Vec<Int_pair>* edges, int max_node){

    cycle_to_id = new Map<std::vector<Int_pair>,int>();
    id_to_cycle = new Map<int,std::vector<Int_pair>>();

    get_ECycles(edges, max_node);

    //init edges in CycleGraph
    Set<Int_pair>* cycle_graph_edges = new Set<Int_pair>();
    for( auto pair : test ){
        auto cycles = pair.second;
        for( auto c1 : cycles ){
            for( auto c2 : cycles ){
                if( c1 == c2 ) continue;
                cycle_graph_edges->insert(Int_pair(cycle_to_id->at(c1),cycle_to_id->at(c2)));
                cycle_graph_edges->insert(Int_pair(cycle_to_id->at(c2),cycle_to_id->at(c1)));
            }
        }
    }

    // std::cout << "This is the cycle graph, should be 0-1 1-2 1-0 1-2" << std::endl;
    // for( auto edge : *cycle_graph_edges ){
    //     std::cout << "<" << edge.first << "," << edge.second << "> ";
    // }
    // std::cout << std::endl;





    get_CCG(cycle_graph_edges,cycle_id);

    for(auto p : CCG ){
        Set<Int_pair> combined_cycles;
        for(auto v : p){
            auto cycle = id_to_cycle->at(v);
            for( auto edge : cycle ) {
                combined_cycles.insert(edge);
            }
        }
        // for( auto edge : combined_cycles ) {
        //     std::cout << "<" << edge.first << "," << edge.second << "> ";
        // }
        // std::cout << std::endl;
        SCSs.insert(combined_cycles);
    }
    delete cycle_graph_edges;
    delete cycle_to_id;
    delete id_to_cycle;
}

Set<Int_pair_SET>* graph_algorithms::clone_SCSs(){
    Set<Int_pair_SET>* C_SCS = new Set<Int_pair_SET>(SCSs);
    return C_SCS;
}

//==========================================
//******************************************
//==========================================
//USE : SPRENGER DAM
bool graph_algorithms::check_SD(SD_decrease_type SD_DEC_TYPE,Vec<Int_SET*>* HeightsOf, Sloped_relation*** h_change_, Vec<Pair<int,int>>* edges_, int max_node_){
    this->h_change_ = h_change_;
    this->HeightsOf = HeightsOf;

    get_SCSs(edges_,max_node_);
    if( SCSs.size() == 0 ) return true;
    else if(SCSs.size() == 1 && ((SCSs.begin())->size() == 1 )){
        return check_descending_SD_singleton();
    }

    for( Set<Int_pair> G : SCSs){
        std::vector<Int_pair>* SCC_edges = new std::vector<Int_pair>(G.begin(), G.end());
        bool result = true;
        result = check_descending_SD(SCC_edges);
        delete SCC_edges;
        if( !result ) {
            return false;
        }
    }
    return true;
}

bool graph_algorithms::enumerate_and_check_SD(  int curr,Vec<int>* node_idxs_, Map<int,int>* rev_node_idxs,Vec<int>* indicies,Vec<Int_pair>* edges){
    if( curr == node_idxs_->size()){
        bool found = false;
        bool valid = true;
        for( auto e: *edges){
            int h1 = indicies->at(rev_node_idxs->at(e.first));
            int h2 = indicies->at(rev_node_idxs->at(e.second));
            int** slope_map = h_change_[e.first][e.second]->repr_matrix;
            if( slope_map[h1][h2] == Undef ){
                valid = false;
                break;
            }
            if( !found && slope_map[h1][h2] ) found = true;
        }
        return (found && valid);
    }
    else{
        Int_SET* heights = (HeightsOf->at(node_idxs_->at(curr)));
        for(int h : *(heights) ){
            indicies->push_back(h);
            if( enumerate_and_check_SD(curr+1,node_idxs_,rev_node_idxs,indicies,edges) ) return true;
            indicies->pop_back();
        }
    }
    return false;
}

bool graph_algorithms::check_descending_SD(Vec<Int_pair>* edges){
    std::vector<int>* node_idxs_ = new std::vector<int>();
    Int_SET* nodes = new Int_SET();
    std::vector<int>* indicies = new std::vector<int>();
    Map<int,int>* rev_node_idx = new Map<int,int>();
    for( auto e : *edges ){
        if (nodes->find(e.first) == nodes->end()) {
            nodes->insert(e.first);
            int idx = node_idxs_->size();
            node_idxs_->push_back(e.first);
            rev_node_idx->insert(Int_pair(e.first,idx));
        }
        if (nodes->find(e.second) == nodes->end()) {
            nodes->insert(e.second);
            int idx = node_idxs_->size();
            node_idxs_->push_back(e.second);
            rev_node_idx->insert(Int_pair(e.second,idx));
        }
    }
    bool res =  enumerate_and_check_SD(0,node_idxs_,rev_node_idx,indicies,edges);
    delete node_idxs_;
    delete nodes;
    delete rev_node_idx;
    delete indicies;
    return res;
}

bool graph_algorithms::check_descending_SD_singleton(){
    bool result = false;
    auto e = *(SCSs.begin()->begin());
    if( e.first == e.second ){
        h_change_[e.first][e.second]->initialize();
        Sloped_relation* S = h_change_[e.first][e.second]->compute_transitive_closure();
        Map<Int_pair,int>* slope_map = S->get_slopes();
        for( auto p_ : *slope_map ){
            if( p_.first.first == p_.first.second &&  p_.second == Downward ){
                result = true;
                break;
            }
        }
        delete S;
    }
    return result;
}

//==========================================
//******************************************
//==========================================
//USE : EXTENDED SPRENGER DAM


Vec< Vec<Set<int>>>* graph_algorithms::get_height_power_sets(int max_h){

    Vec< Vec<Set<int>>>* height_power_sets = new Vec< Vec<Set<int>>>();
    Vec<int> range;
    for( int i = 0 ; i < max_h ; i ++ ) range.push_back(i);

    for( int size = 0 ; size <= max_h ; size ++ ){
        Vec<Set<int>> sub_sets;
        if( size == 0 ){
            height_power_sets->push_back(sub_sets);
            continue;
        }
        unsigned int pow_set_size = pow(2,size);
        int counter, j;
        for (counter = 0; counter < pow_set_size; counter++) {
            Set<int> S;
            for (j = 0; j < size; j++) {
                if (counter & (1 << j)){
                    S.insert(range[j]);
                }
            }
            sub_sets.push_back(S);
        }
        height_power_sets->push_back(sub_sets);
    }
    return height_power_sets;
}


void graph_algorithms::get_functions_for_edge_wrt_height_family( int max , Vec<int>* ind, Set<int>* h1, Set<int>* h2,Vec<Map<int,int>>* mapp,int u, int v,int curr){
    if( curr == max ){
        Map<int,int> m;
        int i = 0;
        for( int h : *h1 ){
            if( (h_change_[u][v]->repr_matrix)[h][ind->at(i)] == Undef) return;
            m.insert(Int_pair(h,ind->at(i++)));
        }
        mapp->push_back(m);
    }
    else{
        for( int fh : *h2 ){
            ind->push_back(fh);
            get_functions_for_edge_wrt_height_family(max,ind,h1,h2,mapp,u,v,curr+1);
            ind->pop_back();
        }
    }
}


void graph_algorithms::get_height_families( Vec<Set<int>>* height_family,Vec< Vec<Set<int>>>* height_sets, int node ){
    
    if( node == max_node_ ){
        
        Vec<Map<int,int>>*** functions = (Vec<Map<int,int>>***)malloc(max_node_ * sizeof(Vec<Map<int,int>>**));
        for( int i = 0 ; i < max_node_ ; i++){
            functions[i] = (Vec<Map<int,int>>**)malloc(max_node_ * sizeof(Vec<Map<int,int>>*));
        }

        Vec<int>* ind = new Vec<int>();
        for( auto edge : *(this->edges) ){
            int size_h = height_family->at(edge.first).size();
            Vec<Map<int,int>>* mapp = new Vec<Map<int,int>>();
            get_functions_for_edge_wrt_height_family(size_h,ind,&(height_family->at(edge.first)),&(height_family->at(edge.second)),mapp,edge.first,edge.second);
            functions[edge.first][edge.second] = mapp;
            
        }
        delete ind;
       
        auto a = Pair< Vec<Set<int>> , Vec<Map<int,int>>***>(Vec<Set<int>>(*height_family) ,functions );
        height_families_and_function_pairs.push_back(a);
    }
    else{
        int number_of_heights = (HeightsOf->at(node))->size();
        for(auto heights : height_sets->at(number_of_heights) ){
            if( heights.size() == 0 ) continue;      // to ignore the empty set subset
            height_family->push_back(heights);
            get_height_families( height_family , height_sets, node + 1 );
            height_family->pop_back();
        }
    }
}


int graph_algorithms::get_extended_graph(Vec<Set<int>>* height_family,Vec<Map<int,int>>* func_fam,Set<Int_pair>* ext_edges,Map<int,Int_pair>* id_to_ext_edge,Map<Int_pair,int>* ext_edge_to_id){
    // create the extended graph 

    Set<Int_pair> guard;
    int ext_node_id = 0;
    int curr_edge_id = 0;
    int ext_node_1;
    int ext_node_2;
    
    for( auto e : *edges ){
        for( auto h : height_family->at(e.first) ){

            Int_pair new_edge_first = Int_pair(e.first,h);
            Int_pair new_edge_second = Int_pair(e.second,(func_fam->at(curr_edge_id)).at(h));

            auto exists = guard.find(new_edge_first);
            if( exists == guard.end() ){
                guard.insert(new_edge_first);
                ext_node_1 = ext_node_id;
                id_to_ext_edge->insert(Pair<int,Int_pair>(ext_node_id,new_edge_first));
                ext_edge_to_id->insert(Pair<Int_pair,int>(new_edge_first,ext_node_id++));
            }
            else ext_node_1 = ext_edge_to_id->at(new_edge_first);


            exists = guard.find(new_edge_second);
            if( exists == guard.end() ){
                guard.insert(new_edge_second);
                ext_node_2=ext_node_id;
                id_to_ext_edge->insert(Pair<int,Int_pair>(ext_node_id,new_edge_second));
                ext_edge_to_id->insert(Pair<Int_pair,int>(new_edge_second,ext_node_id++));

            }
            else ext_node_2 = ext_edge_to_id->at(new_edge_second);

            ext_edges->insert(Int_pair(ext_node_1,ext_node_2));
        }
        curr_edge_id++;
    }
    return ext_node_id;

}


bool graph_algorithms::check_height_family_function_family_decreasing(Vec<Set<int>>* height_family ,Vec<Map<int,int>>* function_family ,Vec<Map<int,int>>*** functions,int curr_edge){
    if( curr_edge == edges->size() ){

        Set<Int_pair>* ext_edges = new Set<Int_pair>();
        Map<int,Int_pair> id_to_ext_edge;
        Map<Int_pair,int> ext_edge_to_id;

        int max_node_ext = get_extended_graph(height_family, function_family,ext_edges,&id_to_ext_edge,&ext_edge_to_id);

        // no edges
        if( ext_edges->size() == 0) return true;
        
        // one edge
        if( max_node_ext == 1 ){
            auto a = id_to_ext_edge.at(ext_edges->begin()->first);
            if ( (h_change_[a.first][a.first]->repr_matrix)[a.second][a.second] != Downward )return false;
            else return true;
        }

        // else create the actual graph and get SCSs
        generic_graph ext_G;
        ext_G.set_edges( new Vec<Int_pair>(ext_edges->begin(),ext_edges->end()),max_node_ext);
        Set<Int_pair_SET>* SCS_Ext = ext_G.get_SCSs();


        // single SCS single node case
        if(SCS_Ext->size() == 1 && ((SCS_Ext->begin())->size() == 1 ) ){
            bool result = false;
            auto e = *((SCS_Ext->begin())->begin());
            int u = (id_to_ext_edge.at(e.first)).first;
            int v = (id_to_ext_edge.at(e.second)).first;
            if( u == v ){
                h_change_[u][v]->initialize();
                Sloped_relation* S = h_change_[u][v]->compute_transitive_closure();
                Map<Int_pair,int>* slope_map = S->get_slopes();
                for( auto p_ : *slope_map ){
                    if( p_.first.first == p_.first.second &&  p_.second == Downward ){
                        result = true;
                        break;
                    }
                }
                delete S;
            }
            return result;
        }


        // normal case
        for(Int_pair_SET scs : *SCS_Ext ){
            if( scs.size() ){
                bool downward = false;
                Set<Int_pair> guard;
                for( auto e : scs ){
                    int u = (id_to_ext_edge.at(e.first)).first;
                    int v = (id_to_ext_edge.at(e.second)).first;
                    guard.insert(Int_pair(u,v));
                    if( h_change_[u][v]->repr_matrix[(id_to_ext_edge.at(e.first)).second][(id_to_ext_edge.at(e.second)).second] == Downward ) downward = true;
                }
                if(!downward && guard.size() == edges->size() ) return false;
            }
        }
        return true;

    }
    else{
        Int_pair edge = edges->at(curr_edge);
        for( Map<int,int> func : *(functions[edge.first][edge.second]) ){
            function_family->push_back(func);
            if( check_height_family_function_family_decreasing(height_family,function_family,functions,curr_edge+1) ) return true;;
            function_family->pop_back();
        }
    }
    
    return false;
}


bool graph_algorithms::check_XSD(int max_node_,Vec<Int_SET*>* HeightsOf, Sloped_relation*** h_change_, int max_h,Vec<Pair<int,int>>* edges){


    // We initialize the internal pointer to elemenate clutter and too-many parameter passing to functions.
    this->HeightsOf = HeightsOf;
    this->h_change_ = h_change_;
    this->max_node_ = max_node_;
    this->edges = edges;


    // Here we get the subsets of the height sets for each size:
    //
    // -- REMEMBER: for each node, we map the heights localy, i.e. if a node has 3 heights they would be {0,1,2}
    //    which works great in optimizing the code and save extra computations.
    //
    // -- This allows us to compute the substets with regards to the amount of heights with one go ( O(heights) instead of O(nodes) ).
    Vec< Vec<Set<int>>>* height_sets = get_height_power_sets(max_h);



    // create families of heights
    // -- height_family     : this is a vector of heights sub-sets for each node (created recursively).
    Vec<Set<int>>* height_family_temp = new Vec<Set<int>>();
    get_height_families(height_family_temp  , height_sets);
    delete height_family_temp;

    if( height_families_and_function_pairs.size() == 0 ) return true;

    

    // get all the SCSs of the heighted graph
    get_SCSs(this->edges,max_node_);

    
    
    // check if each SCS is set descending 
    for( auto scs : SCSs ){

        this->edges = new Vec<Pair<int,int>>(scs.begin(), scs.end());
        
        Vec<Map<int,int>>* function_family = new Vec<Map<int,int>>();
        bool down = false;
        for( auto p : height_families_and_function_pairs ){

            auto height_family = &(p.first);
            auto functions = p.second;


            // for( auto s : *height_family ){
            //     for( int h : s ){
            //         std::cout << h << "," ;
            //     }
            //     std::cout << std::endl;
            // }




            if( check_height_family_function_family_decreasing(height_family, function_family, functions) ){
                down = true;
                break;
            }

        }

        delete this->edges;
        delete function_family;

        if( !down ){
            return false;
        }
    }
    return true;
}












