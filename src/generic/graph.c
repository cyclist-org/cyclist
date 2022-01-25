
#include "graph.hpp"
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
    // delete edges;
    // delete adj;
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
    return enumerate_and_check_SD(0,node_idxs_,rev_node_idx,new std::vector<int>(),edges);
}

bool Graph::check_SD(void){
    if( !get_SCCs(edges) ) return false;
    // print_SCCs();
    std::vector<std::vector<Int_pair>*>* SG = new std::vector<std::vector<Int_pair>*>();
    std::vector<std::vector<Int_pair>*>* SubSG;
    for( auto p : *SCCs){
        if( p.second.size() < 2 ) continue;
        std::vector<Int_pair>* SCC_edges = new std::vector<Int_pair>((p.second).begin(), (p.second).end());
        if( !check_descending_SD(SCC_edges)) return false;
    }
    if( SCCs ){
        delete SCCs;
        SCCs = new Map<int,Int_pair_SET>();
    }
    for( std::vector<Int_pair>* G : *SG){
        SubSG = get_subgraphs(G);
        if( SubSG == nullptr ) continue;
        for( std::vector<Int_pair>* G_p : *SubSG){
            get_SCCs(G_p);
            for( auto p : *SCCs){
                if( p.second.size() < 2 ) continue;
                std::vector<Int_pair>* SCC_edges = new std::vector<Int_pair>((p.second).begin(), (p.second).end());
                if( !check_descending_SD(SCC_edges)) return false;
            }
            if( SCCs ){
                delete SCCs;
                SCCs = new Map<int,Int_pair_SET>();
            }
        }
    }
    return true;
}


std::vector<std::vector<Int_pair>*>* Graph::get_subgraphs(std::vector<Int_pair>* G){
    int n = G->size();
    if( n < 3 ) return nullptr;
    std::vector<std::vector<Int_pair>*>* SG = new std::vector<std::vector<Int_pair>*>();
    int count = std::pow(2, n);
    for (int i = count - 1; i > 0; i--) {
        std::vector<Int_pair>* edges = new std::vector<Int_pair>();
        for (int j = 0; j < n; j++) {
            if ((i & (1 << j)) != 0){
                edges->push_back(G->at(j));
            }
        }
        if( edges->size() < 2){
            delete edges;
            continue;
        }
        for( auto p : *edges){
            std::cout << "<" << p.first << "," << p.second << "> ";
        }
        std::cout << std::endl;
        SG->push_back(edges);
    }
    return SG;
    // int n = G->size();
    // if( n < 3 ) return nullptr;
    // std::vector<std::vector<Int_pair>*>* SG = new std::vector<std::vector<Int_pair>*>();
    // for (int i = 0; i < n; i++) {
    //     std::vector<Int_pair>* edges = new std::vector<Int_pair>();
    //     for (int j = 0; j < n; j++) {
    //         if (j != i ){
    //             edges->push_back(G->at(j));
    //         }
    //     }
    //     for( auto p : *edges){
    //         std::cout << "<" << p.first << "," << p.second << "> ";
    //     }
    //     std::cout << std::endl;
    //     SG->push_back(edges);
    // }
    // return SG;
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

bool Graph::get_SCCs(std::vector<Int_pair>* e){
    bool non_trivial = false;
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
    for( auto SCC : *SCCs ){
        if( SCC.second.size() > 1 ) return true;
    }
    return false;
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
