
#include "graph.hpp"
#include "sloped_relation.hpp"
#include <iostream>
#include <ostream>
#include <cmath>


Graph::Graph(std::vector<Int_pair>* edges,std::vector<Int_SET*>* HeightsOf,Sloped_relation*** h_change_ , int max_node , int max_height){
    this->edges = edges;
    this->max_node = max_node;
    this->max_height = max_height;
    this->HeightsOf = HeightsOf;
    this->h_change_ = h_change_;
    this->node_idxs = new Map<int,int>();
    for( auto e : *edges ){
        if (node_idxs->find(e.first) == node_idxs->end()) {
            int idx = node_idxs->size();
            node_idxs->insert(Int_pair(e.first,idx));
        }
        if (node_idxs->find(e.second) == node_idxs->end()) {
            int idx = node_idxs->size();
            node_idxs->insert(Int_pair(e.second,idx));
        }
    }
}

Graph::~Graph(void){
    delete node_idxs;
    // if( slope_change_functions ){
    //      for( int i = 0 ; i < max_node ; i++ ){
    //          for(int j = 0 ; j < max_node ; j++ ){
    //              delete slope_change_functions[i][j];
    //          }
    //          delete slope_change_functions[i];
    //      }
    //      delete slope_change_functions;
    // }
}

bool Graph::check_SCCs_SD_decreasing(SD_decrease_type SD_DEC_TYPE,std::vector<std::vector<Int_pair>*>* SG,Map<int,Int_pair_SET>* SCCs){
    return check_SCCs_SD_decreasing(SD_DEC_TYPE,nullptr,SG,SCCs);
}

bool Graph::check_SCCs_SD_decreasing(SD_decrease_type SD_DEC_TYPE,std::vector<Int_pair>* edges){
    return check_SCCs_SD_decreasing(SD_DEC_TYPE,edges,nullptr,nullptr);
}

bool Graph::check_SCCs_SD_decreasing(SD_decrease_type SD_DEC_TYPE,std::vector<Int_pair>* edges,std::vector<std::vector<Int_pair>*>* SG,Map<int,Int_pair_SET>* sccs){
    Map<int,Int_pair_SET>* SCCs;
    if ( sccs == nullptr ){
        SCCs = new Map<int,Int_pair_SET>();
        get_SCCs(edges,SCCs);
    }
    else SCCs = sccs;

    for( auto p : *SCCs){
        std::vector<Int_pair>* SCC_edges = new std::vector<Int_pair>((p.second).begin(), (p.second).end());

        bool result = true;
        switch( SD_DEC_TYPE ){
            case XTD: 
                result = check_descending_WeakSD(SCC_edges);
                break;
            case STD:
                result = check_descending_SD(SCC_edges);
                break;
            default: break;
        }
        if (!result) {
            delete SCCs;
            delete SCC_edges;
            return false;
        }
        if( SG ) {
            SG->push_back(SCC_edges);
        } else {
            delete SCC_edges;
        }
    }
    delete SCCs;
    return true;
}

bool Graph::check_SD(SD_decrease_type SD_DEC_TYPE){
    bool result = true;
    Map<int,Int_pair_SET>* SCCs = new Map<int,Int_pair_SET>();
    get_SCCs(edges,SCCs);

    if( SCCs->size() == 0 ) return true;
    else if( SD_DEC_TYPE == STD && SCCs->size() == 1 && ((SCCs->begin())->second).size() == 1 ){
        return check_descending_SD_singleton(SCCs);
    }
    if( SD_DEC_TYPE == XTD ) init_slope_changing_functions();

    auto SG = new std::vector<std::vector<Int_pair>*>();
    if( !check_SCCs_SD_decreasing(SD_DEC_TYPE,SG,SCCs) ) result = false;

    for( std::vector<Int_pair>* G : *SG){

        if( result ){

            int n = G->size();
            int count = 1 << n;

            if( n < 2 ){
                delete G;
                continue;
            }

            for (int i = count - 1; i > 0; i--) {
                std::vector<Int_pair>* SubSG = new std::vector<Int_pair>();
                for (int j = 0; j < n; j++) {
                    if ((i & (1 << j)) != 0){
                        SubSG->push_back(G->at(j));
                    }
                }
                if( result && !check_SCCs_SD_decreasing(SD_DEC_TYPE,SubSG) ) result = false;
                delete SubSG;
            }
        }
        delete G;
    }
    delete SG;
    return result;
}

// std::vector<std::vector<Int_pair>*>* Graph::get_subgraphs(std::vector<Int_pair>* G){
//     int n = G->size();
//     if( n < 2 ) return nullptr;
//     std::vector<std::vector<Int_pair>*>* SG = new std::vector<std::vector<Int_pair>*>();
//     int count = std::pow(2, n);
//     for (int i = count - 1; i > 0; i--) {
//         std::vector<Int_pair>* edges = new std::vector<Int_pair>();
//         for (int j = 0; j < n; j++) {
//             if ((i & (1 << j)) != 0){
//                 edges->push_back(G->at(j));
//             }
//         }
//         SG->push_back(edges);
//     }
//     return SG;
// }

bool Graph::enumerate_and_check_SD(int curr,std::vector<int>* node_idxs_,Map<int,int>* rev_node_idxs,std::vector<int>* indicies,std::vector<Int_pair>* edges){
    if( curr == node_idxs_->size()){
        bool found = false;
        bool valid = true;
        for( auto e: *edges){
            int h1 = indicies->at(rev_node_idxs->at(e.first));
            int h2 = indicies->at(rev_node_idxs->at(e.second));
            Map<Int_pair,int>* slope_map = h_change_[e.first][e.second]->get_slopes();
            auto exists = slope_map->find(Int_pair(h1,h2));
            if(exists == slope_map->end()){
                valid = false;
                break;
            }
            else if(!found && exists->second == Downward){
                found = true;
            }
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

bool Graph::check_descending_SD(std::vector<Int_pair>* edges){
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

bool Graph::check_descending_SD_singleton(Map<int,Int_pair_SET>*  SCCs){
    bool result = false;
    auto edge_set = SCCs->begin()->second;
    std::vector<Int_pair>* SCC_edges = new std::vector<Int_pair>(edge_set.begin(), edge_set.end());
    auto e = SCC_edges->at(0);
    if( e.first == e.second ){
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
    delete SCCs;
    delete SCC_edges;
    return result;
}

void Graph::mark_nodes(int u,std::list<int>* adj,int* disc,int* low,std::stack<int>* st,bool* stackMember){
    static int time = 0;
    disc[u] = low[u] = ++time;
    st->push(u);
    stackMember[u] = true;
    std::list<int>::iterator i;
    for (i = adj[u].begin(); i != adj[u].end(); ++i){
        int v = *i;
        if (disc[v] == -1){
            mark_nodes(v,adj,disc,low,st,stackMember);
            low[u] = std::min(low[u], low[v]);
            
        }
        else if (stackMember[v] == true){
            low[u] = std::min(low[u], disc[v]);
        }
    }
    int w = 0;
    if (low[u] == disc[u]){
        while (st->top() != u){
            w = (int) st->top();
            stackMember[w] = false;
            st->pop();
        }
        w = (int) st->top();
        stackMember[w] = false;
        st->pop();
    }
}

void  Graph::extract_SCC(int u,Map<int,Int_pair_SET>* SCCs,std::list<int>* adj,int* disc,int* low,std::stack<int>* st,bool* stackMember){
    static int time = 0;
    disc[u] = ++time;
    st->push(u);
    stackMember[u] = true;
    std::list<int>::iterator i;
    for (i = adj[u].begin(); i != adj[u].end(); ++i){
        int v = *i;
        if( disc[v] == -1 ) extract_SCC(v,SCCs,adj,disc,low,st,stackMember);
        else if( low[u] == low[v] ){
            auto exists = SCCs->find(low[u]);
            if( exists == SCCs->end()) SCCs->insert(Pair<int,Int_pair_SET>(low[u],Int_pair_SET()));
            (SCCs->at(low[u])).insert(Int_pair(u,v));
        }       
    }
}

void Graph::get_SCCs(std::vector<Int_pair>* e,Map<int,Int_pair_SET>* SCCs){
    int max_node = 9999;
    std::list<int>* adj = new std::list<int>[max_node];
    int* disc = new int[max_node];
    int* low = new int[max_node];
    bool* stackMember = new bool[max_node];
    std::stack<int>* st = new std::stack<int>();
    for( Int_pair p : *e ){
        adj[p.first].push_back(p.second);
    }
    for (int i = 0; i < max_node; i++){
        disc[i] = NIL;
        low[i] = NIL;
        stackMember[i] = false;
    }
    for (int i = 0; i < max_node; i++){
        if (disc[i] == NIL){
            mark_nodes(i,adj,disc,low,st,stackMember);
        }
    }
    for (int i = 0; i < max_node; i++){
        stackMember[i] = false;
        disc[i] = NIL;
        if (disc[i] == NIL){
            extract_SCC(i,SCCs,adj,disc,low,st,stackMember);
        }
    }
    std::vector<int> idx;
    for(auto SCC : *SCCs ){
        if( checked_sccs.find(SCC.second) != checked_sccs.end() ) idx.push_back(SCC.first);
        else checked_sccs.insert(SCC.second);
    }
    for( auto i : idx ){
        SCCs->erase(i);
    }
    delete[] adj;
    delete disc;
    delete low;
    delete st;
    delete stackMember;
}

void Graph::print_SCCs(Map<int,Int_pair_SET>* SCCs){
    std::cout << "\n\n===================================================" << std::endl;
    std::cout << "000000000000000000000000000000000000000000000000000" << std::endl;
    for( auto SCC : *SCCs){
        for( auto p : SCC.second){
            std::cout << "<" << p.first << "," << p.second << "> ";
        }
        std::cout << std::endl;
    }
    std::cout << "000000000000000000000000000000000000000000000000000" << std::endl;
    std::cout << "===================================================" << std::endl;
}

void Graph::get_functions(std::vector<int>* dom,std::vector<int>* cod,std::set<Map<int,int>>* function_vec,Int_SET* heights_second,Int_pair edge){
    if( dom->size() == cod->size() ){
        Map<Int_pair,int>* slope_map = h_change_[edge.first][edge.second]->get_slopes();
        Map<int,int> function = Map<int,int>();

        int size = dom->size();
        for( int i = 0 ; i < size ; i++ ){
            Int_pair p(dom->at(i),cod->at(i));
            if( slope_map->find(p) != slope_map->end() ){
                function.insert(p);
            }
        }
        if( function.size() ) function_vec->insert(function);
    }
    else{
        for( int h2 : *heights_second ){
            cod->push_back(h2);
            get_functions(dom,cod,function_vec,heights_second,edge);
            cod->pop_back();
        }
    }
}

std::set<Map<int,int>>* Graph::enumerate_slope_changing_functions(Int_pair edge){
    std::set<Map<int,int>>* function_vec = new std::set<Map<int,int>>();
    std::vector<int>* dom = new std::vector<int>();
    std::vector<int>* cod = new std::vector<int>();
    Int_SET* heights_first = HeightsOf->at(edge.first);
    Int_SET* heights_second = HeightsOf->at(edge.second);

    for( int h1 : *heights_first ){
        dom->push_back(h1);
    }
    get_functions(dom,cod,function_vec,heights_second,edge);

    // std::cout << "the functions of <" << edge.first << "," << edge.second << "> :" << std::endl;
    // int i = 1;
    // for( auto map : *function_vec ){
    //     std::cout << "\t " << i++ << std::endl << "\t\t";
    //     for( auto pair : map ){
    //         std::cout << pair.first << " ==> " << pair.second << " | ";
    //     }
    //     std::cout << std::endl;
    // }

    return function_vec;
}

void Graph::init_slope_changing_functions(void){
    slope_change_functions = (std::vector<Map<int,int>>***)malloc(sizeof(std::vector<Map<int,int>>**) * max_node);
    for( int i = 0 ; i < max_node ; i++ ){
        slope_change_functions[i] = (std::vector<Map<int,int>>**)malloc(sizeof(std::vector<Map<int,int>>*) * max_node);
    }
    for( auto edge : *edges ){
        auto p = enumerate_slope_changing_functions(edge);
        slope_change_functions[edge.first][edge.second] = new std::vector<Map<int,int>>(p->begin(), p->end());
    }
}

void Graph::get_extended_graphs(int curr,std::vector<int>* idxs,std::vector<Int_pair>* edges){
    if( curr == edges->size()  ){
        auto graph = std::vector<Pair<Int_pair,Int_pair>>();
        for( int e = 0 ; e < edges->size() ; e++ ){
            auto edge = edges->at(e);
            Map<int,int>* f = &slope_change_functions[edge.first][edge.second]->at(idxs->at(e));
            for( auto a : *f ){
                graph.push_back(Pair<Int_pair,Int_pair>(Int_pair(edge.first,a.first),Int_pair(edge.second,a.second)));
            }
        }
        ext_graphs->insert(graph);
    }
    else{
        auto edge = edges->at(curr);
        int size = slope_change_functions[edge.first][edge.second]->size();
        for( int i = 0 ; i < size ; i++ ){
            idxs->push_back(i);
            get_extended_graphs(curr+1,idxs,edges);
            idxs->pop_back();
        }
    }
}

bool Graph::check_set_choice_decrease(std::vector<Pair<Int_pair,Int_pair>>* ext_graph,std::vector<Int_pair>* edges){
    Map<int,Int_pair_SET>* SCCs = new Map<int,Int_pair_SET>();
    Map<Int_pair,int>* node_idxs_ = new Map<Int_pair,int>();
    Map<int,Int_pair>* rev_node_idx = new Map<int,Int_pair>();
    std::vector<Int_pair>* mapped_edges = new std::vector<Int_pair>();
    for( auto e : *ext_graph ){
        if (node_idxs_->find(e.first) == node_idxs_->end()) {
            int idx = node_idxs_->size();
            node_idxs_->insert(Pair<Int_pair,int>(e.first,idx));
            rev_node_idx->insert(Pair<int,Int_pair>(idx,e.first));
        }
        if (node_idxs_->find(e.second) == node_idxs_->end()) {
            int idx = node_idxs_->size();
            node_idxs_->insert(Pair<Int_pair,int>(e.second,idx));
            rev_node_idx->insert(Pair<int,Int_pair>(idx,e.second));
        }
        mapped_edges->push_back(Int_pair(node_idxs_->at(e.first),node_idxs_->at(e.second)));
    }
    get_SCCs(mapped_edges,SCCs);

    // check covers all edges
    for( auto SCC : *SCCs){
        Int_pair_SET SCC_edges = Int_pair_SET();
        bool has_downwards_edge = false;
        for( auto e : SCC.second){
            int e1 = (rev_node_idx->at(e.first)).first;
            int e2 = (rev_node_idx->at(e.second)).first;
            SCC_edges.insert(Int_pair(e1,e2));
            if( !has_downwards_edge ){
                int h1 = (rev_node_idx->at(e.first)).second;
                int h2 = (rev_node_idx->at(e.second)).second;
                Sloped_relation* R = h_change_[e1][e2];
                auto slopes = R->get_slopes();
                if(slopes->at(Int_pair(h1,h2)) == Downward){
                    has_downwards_edge = true;
                }
            }
        }
        if( SCC_edges.size() == edges->size() &&  !has_downwards_edge ){
            delete SCCs;
            delete node_idxs_;
            delete rev_node_idx;
            delete mapped_edges;
            return false;
        }
    }
    delete SCCs;
    delete node_idxs_;
    delete rev_node_idx;
    delete mapped_edges;
    return true;
}

bool Graph::check_descending_WeakSD(std::vector<Int_pair>* edges){
    bool res = false;
    auto idx = new std::vector<int>();
    ext_graphs = new std::set<std::vector<Pair<Int_pair,Int_pair>>>();
    get_extended_graphs(0,idx,edges);
    for( auto v : *ext_graphs ){
        if(check_set_choice_decrease(&v,edges)){
            res = true;
            break;
        }
    }
    delete idx;
    delete ext_graphs;
    return res;
}

