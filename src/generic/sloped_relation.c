#include <iostream>
#include <ostream>
#include <set>
#include <map>

#include "sloped_relation.hpp"
#include "types.c"




Sloped_relation::Sloped_relation( const Sloped_relation& R){
    this->forward_map = new Map<int,Int_pair_SET*>();
    this->backward_map = new Map<int,Int_pair_SET*>();
    this->slope_map = new Map<Int_pair,int>();
    this->max_height = R.max_height;
    this->min_height = R.min_height;
    for( auto pair : *(R.forward_map) ){
        (this->forward_map)->insert(Pair<int,Int_pair_SET*>(pair.first,new Int_pair_SET(*(pair.second))));
    }
    for( auto pair : *(R.backward_map) ){
        this->backward_map->insert(Pair<int,Int_pair_SET*>(pair.first,new Int_pair_SET(*(pair.second))));
    }
    for( auto pair : *(R.slope_map) ){
        this->slope_map->insert(pair);
    }
}

Sloped_relation::Sloped_relation( Sloped_relation&& R){
    this->forward_map = R.forward_map;
    this->backward_map = R.backward_map;
    this->slope_map = R.slope_map;
    this->max_height = R.max_height;
    this->min_height = R.min_height;
    R.forward_map = nullptr;
    R.backward_map = nullptr;
    R.slope_map = nullptr;
}

Sloped_relation& Sloped_relation::operator=(const Sloped_relation& R){
    if( this != &R ){
        clear();
        this->forward_map = new Map<int,Int_pair_SET*>();
        this->backward_map = new Map<int,Int_pair_SET*>();
        this->slope_map = new Map<Int_pair,int>();
        if( (R.forward_map) != 0 && R.backward_map != 0){
            for( auto pair : *(R.forward_map)){
                (this->forward_map)->insert(Pair<int,Int_pair_SET*>(pair.first,new Int_pair_SET(*(pair.second))));
            }
            for( auto pair : *(R.backward_map)){
                (this->backward_map)->insert(Pair<int,Int_pair_SET*>(pair.first,new Int_pair_SET(*(pair.second))));
            }
            for( auto pair : *(R.slope_map)){
                (this->slope_map)->insert(pair);
            }
        }
    }
    return *this;
}

Sloped_relation& Sloped_relation::operator=(Sloped_relation&& R){
    if( this != &R ){
        clear();
        this->forward_map = R.forward_map;
        this->backward_map = R.backward_map;
        this->slope_map = R.slope_map;
        R.forward_map = nullptr;
        R.backward_map = nullptr;
        R.slope_map = nullptr;
    }
    return *this;
}

void Sloped_relation::add(int h1, int h2, slope s) {

    if( min_height == 0 ) min_height = h1;
    if( max_height < h1 ) max_height = h1;
    if( max_height < h2 ) max_height = h2;
    if( min_height > h1) min_height = h1;
    if( min_height > h2) min_height = h2;


    
    // Add forward mapping
    auto exists_h1 = forward_map->find(h1);
    if( exists_h1 == forward_map->end() ){
        forward_map->insert(Pair<int,Int_pair_SET*>(h1,new Int_pair_SET()));
    }
    Int_pair h1_other(h2, 1-s);
    int h1_count = (forward_map->at(h1))->count(h1_other);
    // If slope is Downward, we want to overwrite an existing Stay mapping
    if( s == Downward ){
        (forward_map->at(h1))->erase(h1_other);
    }
    // Don't overwrite an existing Downward mapping with a Stay
    if( s == Downward || h1_count == 0 ){
        (forward_map->at(h1))->insert(Int_pair(h2, s));
    }

    // Add backward mapping
    auto exists_h2 = backward_map->find(h2);
    if( exists_h2 == backward_map->end() ){
        backward_map->insert(Pair<int,Int_pair_SET*>(h2,new Int_pair_SET()));
    }
    Int_pair h2_other(h1, 1-s);
    int h2_count = (backward_map->at(h2))->count(h2_other);
    // If slope is Downward, we want to overwrite an existing Stay mapping
    if( s == Downward ){
        (backward_map->at(h2))->erase(h2_other);
    }
    // Don't overwrite an existing Downward mapping with a Stay
    if( s == Downward || h2_count == 0 ){
        (backward_map->at(h2))->insert(Int_pair(h1, s));
    }

    Int_pair p3(h1,h2);
    auto exists_s = slope_map->find(p3);
    if( exists_s == slope_map->end() ){
        slope_map->insert(Pair<Int_pair,int>(p3,s));
    }
    // Don't overwrite an existing Downward mapping with a Stay
    else if( s == Downward || slope_map->at(p3) != Downward ){
        slope_map->at(p3) = s;
    }
}
  
Int_pair_SET* Sloped_relation::get_forward_slopes(int h){
    auto ret = (this->forward_map)->find(h);
    if( ret == (this->forward_map)->end() ) {
        return nullptr;
    }
    return ret->second;
}

Int_pair_SET* Sloped_relation::get_backward_slopes(int h){
    auto ret = (this->backward_map)->find(h);
    if( ret == (this->backward_map)->end() ) {
        return nullptr;
    }
    return ret->second;
}

Map<Int_pair,int>* Sloped_relation::get_slopes(void){
    return slope_map;
}


Sloped_relation* Sloped_relation::compose(Sloped_relation* other){
    Map<int,Int_pair_SET*>* composed_forward_map = new Map<int,Int_pair_SET*>();
    Map<int,Int_pair_SET*>* composed_backward_map = new Map<int,Int_pair_SET*>();
    Map<Int_pair,int>* composed_slope_map = new Map<Int_pair,int>();
    Int_pair_SET* slopes_h1_h2;
    Int_pair_SET* slopes_h2_h3;
    for( auto p1 : *(this->forward_map) ){
        for( auto p2 : *(other->backward_map) ){
            slopes_h1_h2 = p1.second;
            slopes_h2_h3 = p2.second;
            int h1 = p1.first;
            int h3 = p2.first;
            int slope = Stay;
            bool found = false;
            Int_pair forward;
            Int_pair backward;
            for( Int_pair p3 : *slopes_h1_h2 ){
                Int_pair p3_down(p3.first,Downward);
                Int_pair p3_stay(p3.first,Stay);
                auto exists = slopes_h2_h3->find(p3_down);
                if( exists == slopes_h2_h3->end() ){
                    exists = slopes_h2_h3->find(p3_stay);
                    if( exists == slopes_h2_h3->end() ) continue;
                    else slope = p3.second;
                }
                else slope = Downward;
                found = true;
                if( slope == Downward ) break;
            }
            if( found ){
                forward = Int_pair(h3,slope);
                backward = Int_pair(h1,slope);
                auto exists_h1 = composed_forward_map->find(h1);
                if( exists_h1 == composed_forward_map->end()) {
                    composed_forward_map->insert(Pair<int,Int_pair_SET*>(h1,new Int_pair_SET()));
                }
                auto exists_h3 = composed_backward_map->find(h3);
                if( exists_h3 == composed_backward_map->end() ) {
                    composed_backward_map->insert(Pair<int,Int_pair_SET*>(h3,new Int_pair_SET()));
                }
                (composed_forward_map->at(h1))->insert(forward);
                (composed_backward_map->at(h3))->insert(backward);
                composed_slope_map->insert(Pair<Int_pair,int>(Int_pair(h1,h3),slope));
            }
        }
    }
    int haha = this->max_height > other->max_height ? this->max_height : other->max_height;
    return new Sloped_relation(composed_forward_map,composed_backward_map,composed_slope_map,haha);
}

Sloped_relation::~Sloped_relation(void){
    clear();
}


Sloped_relation Sloped_relation::compute_transitive_closure(void){
    Sloped_relation R(*this);
    bool done = false;
    while( !done ){
        Sloped_relation* R_prime = compose(&R);
        done = true;
        Map<Int_pair,int>* R_slopes = R.get_slopes();
        Map<Int_pair,int>* R_prime_slopes = R_prime->get_slopes();
        for( auto R_p : *(R_prime_slopes) ){
            auto exists = R_slopes->find(R_p.first);
            if( exists == R_slopes->end() ){
                done = false;
                R.add(R_p.first.first,R_p.first.second,(slope)R_p.second);
            }
            else if( R_p.second < exists->second ){
                done = false;
                R.add(R_p.first.first,R_p.first.second,(slope)R_p.second);
            }
        }
        delete R_prime;
    }
    return R;
}

bool operator< (const Sloped_relation& R, const Sloped_relation& L){
    return (R.slope_map < L.slope_map);
}


bool operator== (const Sloped_relation& R, const Sloped_relation& L){
    if( (R.slope_map)->size() != (L.slope_map)->size() ){
        return false;
    }
    for( auto r : *(R.slope_map) ){
        auto exists = (L.slope_map)->find(r.first);
        if( exists == (L.slope_map)->end() ) return false;
        if( r.second != exists->second ) return false;
    }
    return true;
}
int Sloped_relation::size(void){
    return slope_map->size();
}

void Sloped_relation::clear(void){
    for( auto pair : *(forward_map) ){
        if( pair.second ) delete pair.second;
    }
    for( auto pair : *(backward_map) ){
        if( pair.second ) delete pair.second;
    }
    delete forward_map;
    delete backward_map;
    delete slope_map;
}

void Sloped_relation::print_(void){
    std::cout << "--------------------------------------------" << std::endl;
    std::cout << "Size of relation: " << this->size() << std::endl;
    std::cout << "--------------------------------------------" << std::endl;
    for( auto r : *(slope_map) ){
        std::cout << "source height: " << r.first.first << std::endl;;
        std::cout << "      dist:" << r.first.second << " slope:" << r.second << std::endl;
    }
    std::cout << "============================================" << std::endl;
}


comparison Sloped_relation::compare(const Sloped_relation& lhs){
    comparison result = initial;
    for( auto pair : *(lhs.slope_map) ){
        auto exists = (this->slope_map)->find(pair.first);
        if( exists == (this->slope_map)->end() ){
            if( result == initial ) result = less;
            else if( result == greq ) return noncomp;
        }
        else if( exists->second >= pair.second ){
            if( result == initial ) result = greq;
            else if( result == less ) return noncomp;
        }
        else if( exists->second < pair.second ){
            if( result == initial ) result = less;
            else if( result == greq ) return noncomp;
        }
    }
    for( auto pair : *(this->slope_map) ){
        if( result == initial ) return greq;
        auto exists = (lhs.slope_map)->find(pair.first);
        if( exists == (this->slope_map)->end() ){
            if( result == less) return noncomp; 
        }
    }
    return result;
}


void fill_order( int v, int* visited, std::stack<int>* s,int** g ,int n){
    visited[v] = 1;
    for(int i = 0; i < n; ++i)
        if(g[v][i] != 2)
            if(!visited[i])
                fill_order(i, visited, s,g,n);
    s->push(v);
}


void find_scc(int v, int* visited, int** g,int n,bool* b){
    visited[v] = 1;
    for (int i = 0 ; i < n; ++i)
        if(g[i][v] != 2 ){
            if(g[i][v] == 1) *b = true;
            if (visited[i] == 0)
                find_scc(i, visited,g,n,b);
        }
}

bool Sloped_relation::has_downward_SCC(void){
    int** g = (int**)malloc( (max_height + 1) * sizeof(int*));
    int* visited = (int*)malloc((max_height + 1) * sizeof(int));
    for( int i = 0 ; i < max_height + 1 ; i++ ){
        g[i] = (int*)malloc((max_height + 1) * sizeof(int));
        visited[i] = 0;
        for( int j = 0 ; j < max_height + 1 ; j++ ){
            g[i][j] = 2;
        }
    }

    for( Pair<Int_pair,int> pair : *(this->slope_map) ){
        g[(pair.first).first][(pair.first).second] = pair.second;
    }
    std::stack<int> s;
    for(int i = min_height; i < max_height + 1; i++)
        if(visited[i] == 0)
            fill_order(i, visited, &s,g,max_height+1);

    for( int i = 0 ; i < max_height + 1; i++) {
        visited[i] = 0;
    }
    bool down = false;
    while (s.empty() == false){
        int v = s.top();
        s.pop();
        if (visited[v] == 0){
            find_scc(v, visited,g,max_height+1,&down);
        }
        if( down ){
            for( int i = 0 ; i < max_height + 1 ; i++ ){
                delete g[i];
            }
            delete g;
            delete visited;
            return true;
        }
    }
    
   
    for( int i = 0 ; i < max_height + 1 ; i++ ){
        delete g[i];
    }
    delete g;
    delete visited;


    return false;
}


//enum comparison {greq,less,noncomp,initial};
