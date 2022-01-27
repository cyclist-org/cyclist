
#include "graph.hpp"
#include "sloped_relation.hpp"
#include <iostream>
#include <ostream>
#include <cmath>


Graph::Graph(std::vector<Int_pair>* edges,std::vector<Int_SET*>* HeightsOf,Sloped_relation*** h_change_ , int max_node , int max_height){
    this->SCCs = new Map<int,Int_pair_SET>();
    this->edges = edges;
    this->max_node = max_node;
    this->max_height = max_height;
    this->HeightsOf = HeightsOf;
    this->h_change_ = h_change_;
    disc = new int[max_node];
    low = new int[max_node];
    stackMember = new bool[max_node];
    st = new std::stack<int>();
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
    if( SCCs ) delete SCCs;
    delete node_idxs;
    delete disc;
    delete low;
    delete st;
    delete stackMember;
}

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

bool Graph::check_descending_SD_singleton(void){
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
    if( SCCs ){
        delete SCCs;
        SCCs = new Map<int,Int_pair_SET>();
    }
    delete SCC_edges;
    return result;
}

bool Graph::check_SCCs_SD_decreasing(std::vector<std::vector<Int_pair>*>* SG){
    return check_SCCs_SD_decreasing(nullptr,SG);
}

bool Graph::check_SCCs_SD_decreasing(std::vector<Int_pair>* edges){
    return check_SCCs_SD_decreasing(edges,nullptr);
}

bool Graph::check_SCCs_SD_decreasing(std::vector<Int_pair>* edges,std::vector<std::vector<Int_pair>*>* SG){
    if( edges ) get_SCCs(edges);
    for( auto p : *SCCs){
        std::vector<Int_pair>* SCC_edges = new std::vector<Int_pair>((p.second).begin(), (p.second).end());
        if( !check_descending_SD(SCC_edges)){
            delete SCC_edges;
            return false;
        }
        if( SG ) SG->push_back(SCC_edges);
        else delete SCC_edges;
    }
    if( SCCs ){
        delete SCCs;
        SCCs = new Map<int,Int_pair_SET>();
    }
    return true;
}

bool Graph::check_SD(void){
    bool result = true;
    get_SCCs(edges);

    if( SCCs->size() == 0 ) return true;
    else if( SCCs->size() == 1 && ((SCCs->begin())->second).size() == 1 ){
        return check_descending_SD_singleton();
    }

    auto SG = new std::vector<std::vector<Int_pair>*>();
    if( !check_SCCs_SD_decreasing(SG) ) result = false;


    for( std::vector<Int_pair>* G : *SG){

        if( result ){

            int n = G->size();
            if( n < 2 ){
                delete G;
                continue;
            }
            int count = std::pow(2, n);
            
            for (int i = count - 1; i > 0; i--) {
                std::vector<Int_pair>* SubSG = new std::vector<Int_pair>();
                for (int j = 0; j < n; j++) {
                    if ((i & (1 << j)) != 0){
                        SubSG->push_back(G->at(j));
                    }
                }
                if( result && !check_SCCs_SD_decreasing(SubSG) ) result = false;
                delete SubSG;
            }

        }
        delete G;
    }
    delete SG;
    return result;
}

std::vector<std::vector<Int_pair>*>* Graph::get_subgraphs(std::vector<Int_pair>* G){
    int n = G->size();
    if( n < 2 ) return nullptr;
    std::vector<std::vector<Int_pair>*>* SG = new std::vector<std::vector<Int_pair>*>();
    int count = std::pow(2, n);
    for (int i = count - 1; i > 0; i--) {
        std::vector<Int_pair>* edges = new std::vector<Int_pair>();
        for (int j = 0; j < n; j++) {
            if ((i & (1 << j)) != 0){
                edges->push_back(G->at(j));
            }
        }
        SG->push_back(edges);
    }
    return SG;
}

void Graph::mark_nodes(int u){
    static int time = 0;
    disc[u] = low[u] = ++time;
    st->push(u);
    stackMember[u] = true;
    std::list<int>::iterator i;
    for (i = adj[u].begin(); i != adj[u].end(); ++i){
        int v = *i;
        if (disc[v] == -1){
            mark_nodes(v);
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

void  Graph::extract_SCC(int u){
    static int time = 0;
    disc[u] = ++time;
    st->push(u);
    stackMember[u] = true;
    std::list<int>::iterator i;
    for (i = adj[u].begin(); i != adj[u].end(); ++i){
        int v = *i;
        if( disc[v] == -1 ) extract_SCC(v);
        else if( low[u] == low[v] ){
            auto exists = SCCs->find(low[u]);
            if( exists == SCCs->end()) SCCs->insert(Pair<int,Int_pair_SET>(low[u],Int_pair_SET()));
            (SCCs->at(low[u])).insert(Int_pair(u,v));
        }       
    }
}

void Graph::get_SCCs(std::vector<Int_pair>* e){
    this->adj = new std::list<int>[max_node];
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
            mark_nodes(i);
        }
    }
    for (int i = 0; i < max_node; i++){
        stackMember[i] = false;
        disc[i] = NIL;
        if (disc[i] == NIL){
            extract_SCC(i);
        }
    }
    delete[] this->adj;
}

void Graph::print_SCCs(void){
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
