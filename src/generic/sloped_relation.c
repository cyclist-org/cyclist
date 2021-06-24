#include <iostream>
#include <ostream>
#include <set>
#include <map>

#include "sloped_relation.hpp"
#include "types.c"

void Sloped_relation::add(int h1, int h2, slope s) {
    // Add forward mapping
    Int_pair p1(h2, s);
    if(forward_map.find(h1) == forward_map.end()){
        forward_map.insert(std::pair<int,Int_pair_SET>(h1,Int_pair_SET()));
    }
    forward_map.at(h1).insert(p1);

    // Add backward mapping
    Int_pair p2(h1, s);
    if(backward_map.find(h2) == backward_map.end()){
        backward_map.insert(std::pair<int,Int_pair_SET>(h2,Int_pair_SET()));
    }
    backward_map.at(h2).insert(p2);
}
  
Int_pair_SET* Sloped_relation::get_forward_slopes(int h){
    auto ret = this->forward_map.find(h);
    if ( ret == this->forward_map.end() ) {
        return 0;
    }
    return &(this->forward_map.at(h));
}

Int_pair_SET* Sloped_relation::get_backward_slopes(int h){
    auto ret = this->backward_map.find(h);
    if ( ret == this->backward_map.end() ) {
        return 0;
    }
    return &(this->backward_map.at(h));
}

Sloped_relation Sloped_relation::compose(Sloped_relation* other){
    Map<int,Int_pair_SET> composed_forward_map;
    Map<int,Int_pair_SET> composed_backward_map;
    Int_pair_SET slopes_h1_h2;
    Int_pair_SET slopes_h2_h3;
    for( auto p1 : this->forward_map){
        for( auto p2 : other->backward_map){
            slopes_h1_h2 = p1.second;
            slopes_h2_h3 = p2.second;
            int h1 = p1.first;
            int h3 = p2.first;
            for( Int_pair p3 : slopes_h1_h2){
                for( Int_pair p4 : slopes_h2_h3){
                    if(p3.first == p4.first){ 
                        auto p = composed_forward_map.find(h1);
                        if ( p == composed_forward_map.end()) {
                            composed_forward_map.insert(std::pair<int,std::set<std::pair<int,int>>>(h1,std::set<std::pair<int,int>>()));
                        }
                        auto pp = composed_backward_map.find(h3);
                        if ( pp == composed_backward_map.end() ) {
                            composed_backward_map.insert(std::pair<int,std::set<std::pair<int,int>>>(h3,std::set<std::pair<int,int>>()));
                        }
                        int slope = (p3.second == Downward || p4.second == Downward) ? Downward : Stay;
                        Int_pair a1(h3,slope);
                        Int_pair a2(h1,slope);
                        composed_forward_map.at(h1).insert(a1);
                        composed_backward_map.at(h3).insert(a2);
                    }
                }
            }
        }
    }
    return Sloped_relation(composed_forward_map,composed_backward_map);
}

Sloped_relation Sloped_relation::compute_transitive_closure(void){
    Sloped_relation R = *this;
    Sloped_relation R_prime;
    bool done = false;
    bool first_iter = true;
    while ( !done ){
        R_prime = compose(&R);
        done = true;
        if ( R < R_prime ){
            done = false;
            R = R_prime;
        }
    }
    return R;
}

bool operator<= (const Sloped_relation& R, const Sloped_relation& L){
    for( const auto& pairR : R.forward_map){
        for( const auto& pairL : L.forward_map){
            if(pairR.first != pairL.first) continue;
            for( const auto& a : pairR.second){
                for( const auto& b : pairL.second){
                    if(a.first != b.first) continue;
                    else if (a.second > b.second) return false;
                }
            }
        }
    }
    return true;
}

bool operator< (const Sloped_relation& R, const Sloped_relation& L){
    for( const auto& pairR : R.forward_map){
        for( const auto& pairL : L.forward_map){
            if(pairR.first != pairL.first) continue;
            for( const auto& a : pairR.second){
                for( const auto& b : pairL.second){
                    if(a.first != b.first) continue;
                    else if (a.second >= b.second) return false;
                }
            }
        }
    }
    return true;
}

bool operator== (const Sloped_relation& R, const Sloped_relation& L){
    if( R.forward_map != L.forward_map) {
        return false;
    }
    if( R.backward_map != L.backward_map){
        return false;
    }
    return true;
}


void Sloped_relation::print_(void){
    std::cout << "--------------------------------------------" << std::endl;
    for( const auto& pair : forward_map){
        std::cout << "source height: " << pair.first << std::endl;;
        for(const auto& p : pair.second){
            std::cout << "      dist:" << p.first << " slope:" << p.second << std::endl;
        }
    }
    std::cout << "============================================" << std::endl;
}
