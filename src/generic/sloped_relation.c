#include <iostream>
#include <ostream>
#include <set>
#include <map>

#include "sloped_relation.hpp"
#include "types.c"




Sloped_relation Sloped_relation::compute_transitive_closure(void){
    Sloped_relation R(*this);
    bool done = false;
    while( !done ){
        Sloped_relation R_prime = compose(&R);
        done = true;
        if( R < R_prime ){
            done = false;
            R = R_prime;
        }
    }
    return R;
}


Sloped_relation::Sloped_relation( const Sloped_relation& R){
    this->forward_map = new Map<int,Int_pair_SET*>();
    this->backward_map = new Map<int,Int_pair_SET*>();
    for( auto pair : *(R.forward_map) ){
        (this->forward_map)->insert(Pair<int,Int_pair_SET*>(pair.first,new Int_pair_SET(*(pair.second))));
    }
    for( auto pair : *(R.backward_map) ){
        this->backward_map->insert(Pair<int,Int_pair_SET*>(pair.first,new Int_pair_SET(*(pair.second))));
    }
}

Sloped_relation::Sloped_relation( Sloped_relation&& R){
    this->forward_map = R.forward_map;
    this->backward_map = R.backward_map;
    R.forward_map = nullptr;
    R.backward_map = nullptr;
}

Sloped_relation& Sloped_relation::operator=(const Sloped_relation& R){
    if( this != &R ){
        clear();
        this->forward_map = new Map<int,Int_pair_SET*>();
        this->backward_map = new Map<int,Int_pair_SET*>();
        if( (R.forward_map) != 0 && R.backward_map != 0){
            for( auto pair : *(R.forward_map)){
                (this->forward_map)->insert(Pair<int,Int_pair_SET*>(pair.first,new Int_pair_SET(*(pair.second))));
            }
            for( auto pair : *(R.backward_map)){
                (this->backward_map)->insert(Pair<int,Int_pair_SET*>(pair.first,new Int_pair_SET(*(pair.second))));
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
        R.forward_map = nullptr;
        R.backward_map = nullptr;
    }
    return *this;
}



void Sloped_relation::add(int h1, int h2, slope s) {
    // Add forward mapping
    Int_pair p1(h2, s);
    auto exists_h1 = forward_map->find(h1);
    if( exists_h1 == forward_map->end() ){
        forward_map->insert(Pair<int,Int_pair_SET*>(h1,new Int_pair_SET()));
    }
    (forward_map->at(h1))->insert(p1);

    // Add backward mapping
    Int_pair p2(h1, s);
    auto exists_h2 = backward_map->find(h2);
    if( exists_h2 == backward_map->end() ){
        backward_map->insert(Pair<int,Int_pair_SET*>(h2,new Int_pair_SET()));
    }
    (backward_map->at(h2))->insert(p2);
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

Sloped_relation Sloped_relation::compose(Sloped_relation* other){
    Map<int,Int_pair_SET*>* composed_forward_map = new Map<int,Int_pair_SET*>();
    Map<int,Int_pair_SET*>* composed_backward_map = new Map<int,Int_pair_SET*>();
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
            }
        }
    }
    return Sloped_relation(composed_forward_map,composed_backward_map);
}

Sloped_relation::~Sloped_relation(void){
    clear();
}

bool operator< (const Sloped_relation& R, const Sloped_relation& L){
    if( R.forward_map == 0 || L.forward_map == 0 ) return false;
    else if( (R.forward_map)->size() == 0 || (L.forward_map)->size() == 0 ){
        return false;
    }
    for( const auto& pairR : *(R.forward_map) ){
        for( const auto& pairL : *(L.forward_map) ){
            if( pairR.first != pairL.first ) continue;
            for( const auto& a : *(pairR.second) ){
                for( const auto& b : *(pairL.second) ){
                    if( a.first != b.first ) continue;
                    else if (a.second >= b.second) return false;
                }
            }
        }
    }
    return true;
}

bool operator== (const Sloped_relation& R, const Sloped_relation& L){
    if( (R.forward_map)->size() != (L.forward_map)->size()){
        return false;
    }
    for( auto r : *(R.forward_map) ){
        auto exists = (L.forward_map)->find(r.first);
        if( exists == (L.forward_map)->end() ) return false;
        if( *(r.second) != *(exists->second) ) return false;
    }
    return true;
}
int Sloped_relation::size(void){
    return forward_map->size();
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
}

void Sloped_relation::print_(void){
    std::cout << "--------------------------------------------" << std::endl;
    for( const auto& pair : *(forward_map) ){
        std::cout << "source height: " << pair.first << std::endl;;
        for( const auto& p : *(pair.second) ){
            std::cout << "      dist:" << p.first << " slope:" << p.second << std::endl;
        }
    }
    std::cout << "============================================" << std::endl;
}
