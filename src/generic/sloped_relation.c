#include <cassert>
#include <iostream>
#include <ostream>
#include <set>
#include <map>

#include "sloped_relation.hpp"
#include "types.c"


//==================================================================
// USE : ON CREATION
Sloped_relation::Sloped_relation(int num_src_heights, int num_dst_heights){
    this->num_src_heights = num_src_heights;
    this->num_dst_heights = num_dst_heights;

    repr_matrix = (int**)malloc(sizeof(int*) * num_src_heights);

    for( int i = 0 ; i < num_src_heights ; i++ ){
        repr_matrix[i] = (int*)malloc(sizeof(int) * num_dst_heights);
        for( int j = 0 ; j < num_dst_heights ; j++ ){
            repr_matrix[i][j] = Undef;
        }
    }

    this->forward_map = new Map<int,Int_pair_SET*>();
    this->backward_map = new Map<int,Int_pair_SET*>();
    this->slope_map = new Map<Int_pair,int>();
}

Sloped_relation::Sloped_relation(const Sloped_relation& R){
    this->forward_map = new Map<int,Int_pair_SET*>();
    this->backward_map = new Map<int,Int_pair_SET*>();
    this->slope_map = new Map<Int_pair,int>();
    this->num_src_heights = R.num_src_heights;
    this->num_dst_heights = R.num_dst_heights;
    this->initialized = R.initialized;
    // Question: why do we not create a repr_matrix this new relation?
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

Sloped_relation::Sloped_relation(Sloped_relation&& R){
    this->forward_map = R.forward_map;
    this->backward_map = R.backward_map;
    this->slope_map = R.slope_map;
    this->num_src_heights = R.num_src_heights;
    this->num_dst_heights = R.num_dst_heights;
    this->initialized = R.initialized;

    R.forward_map = nullptr;
    R.backward_map = nullptr;
    R.slope_map = nullptr;

    // Question: why do we not copy the repr_matrix pointer and set to null in R?
}

Sloped_relation& Sloped_relation::operator=(const Sloped_relation& R){
    if( this != &R ){
        clear();
        this->forward_map = new Map<int,Int_pair_SET*>();
        this->backward_map = new Map<int,Int_pair_SET*>();
        this->slope_map = new Map<Int_pair,int>();
        this->num_src_heights = R.num_src_heights;
        this->num_dst_heights = R.num_dst_heights;
        this->initialized = true;
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
        // Question: why do we not deal with repr_matrix here?
    }
    return *this;
}

Sloped_relation& Sloped_relation::operator=(Sloped_relation&& R){
    if( this != &R ){
        clear();
        this->forward_map = R.forward_map;
        this->backward_map = R.backward_map;
        this->slope_map = R.slope_map;
        this->num_src_heights = R.num_src_heights;
        this->num_dst_heights = R.num_dst_heights;
        this->initialized = R.initialized;
        R.forward_map = nullptr;
        R.backward_map = nullptr;
        R.slope_map = nullptr;
        // Question: why do we not copy the repr_matrix pointer and set to null in R?
    }
    return *this;
}

//==================================================================
// USE : ON DISTRUCTION
Sloped_relation::~Sloped_relation(void){
    clear();
}


void Sloped_relation::clear(void){
    // Question: why do we not delete repr_matrix here?

    if( repr_matrix ){
        for( int i = 0 ; i < num_src_heights ; i++ ){
            free(repr_matrix[i]);
        }
        free(repr_matrix);
    }

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

//==================================================================
// USE : GET AND SET
void Sloped_relation::initialize(void){
    if( initialized ) return;
    for( int i = 0 ; i < num_src_heights ; i++ ){
        for( int j = 0 ; j < num_dst_heights ; j++ ){
            if( repr_matrix[i][j] != Undef ){
                auto exists = forward_map->find(i);
                if( exists == forward_map->end() ){
                    forward_map->insert(Pair<int,Int_pair_SET*>(i,new Int_pair_SET()));
                }
                (forward_map->at(i))->insert(Int_pair(j,(int)repr_matrix[i][j]));
                exists = backward_map->find(j);
                 if( exists == backward_map->end() ){
                    backward_map->insert(Pair<int,Int_pair_SET*>(j,new Int_pair_SET()));
                }
                (backward_map->at(j))->insert(Int_pair(i,(int)repr_matrix[i][j]));
                slope_map->insert(Pair<Int_pair,int>(Int_pair(i,j),(int)repr_matrix[i][j]));
            }
        }
        // free(repr_matrix[i]);
    }
    // free(repr_matrix);
    initialized = true;
    return;
}

void Sloped_relation::add(int h1, int h2, slope s) {

    assert (h1 < num_src_heights);
    assert (h2 < num_dst_heights);

    if( !initialized ){
        if( repr_matrix[h1][h2] < s){
            repr_matrix[h1][h2] = s;
        }
    } 
    else {
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

int Sloped_relation::size(void) const{
    return slope_map->size();
}

slope Sloped_relation::get_slope(int src_h, int dst_h) {
    if (src_h < this->num_src_heights && dst_h < this->num_dst_heights) {
        return (slope) this->repr_matrix[src_h][dst_h];
    }
    return Undef;
}

//==================================================================
// USE : USEFUL COMPUTATION METHODS

Sloped_relation* Sloped_relation::compose(Sloped_relation& other){
    Map<int,Int_pair_SET*>* composed_forward_map = new Map<int,Int_pair_SET*>();
    Map<int,Int_pair_SET*>* composed_backward_map = new Map<int,Int_pair_SET*>();
    Map<Int_pair,int>* composed_slope_map = new Map<Int_pair,int>();
    Int_pair_SET* slopes_h1_h2;
    Int_pair_SET* slopes_h2_h3;
    for( auto p1 : *(this->forward_map) ){
        for( auto p2 : *(other.backward_map) ){
            slopes_h1_h2 = p1.second;
            slopes_h2_h3 = p2.second;
            int h1 = p1.first;
            int h3 = p2.first;
            int slope = Stay;
            bool found = false;
            // Should check here which of slopes_h1_h2 and slopes_h2_h3 is
            // smaller, and iterate through that one
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
                auto exists_h1 = composed_forward_map->find(h1);
                if( exists_h1 == composed_forward_map->end()) {
                    composed_forward_map->insert(Pair<int,Int_pair_SET*>(h1,new Int_pair_SET()));
                }
                auto exists_h3 = composed_backward_map->find(h3);
                if( exists_h3 == composed_backward_map->end() ) {
                    composed_backward_map->insert(Pair<int,Int_pair_SET*>(h3,new Int_pair_SET()));
                }
                (composed_forward_map->at(h1))->insert(Int_pair(h3,slope));
                (composed_backward_map->at(h3))->insert(Int_pair(h1,slope));
                composed_slope_map->insert(Pair<Int_pair,int>(Int_pair(h1,h3),slope));
            }
        }
    }
    return
        new Sloped_relation(composed_forward_map,
                            composed_backward_map,
                            composed_slope_map,
                            this->num_src_heights,
                            other.num_dst_heights);
}

Sloped_relation* Sloped_relation::compute_transitive_closure(void){
    Sloped_relation* R = new Sloped_relation(*this);
    bool done = false;
    while( !done ){
        Sloped_relation* R_prime = compose(*R);
        done = true;
        Map<Int_pair,int>* R_slopes = R->get_slopes();
        Map<Int_pair,int>* R_prime_slopes = R_prime->get_slopes();
        for( auto R_p : *(R_prime_slopes) ){
            auto exists = R_slopes->find(R_p.first);
            if( exists == R_slopes->end() ){
                done = false;
                R->add(R_p.first.first,R_p.first.second,(slope)R_p.second);
            }
            else if( R_p.second < exists->second ){
                done = false;
                R->add(R_p.first.first,R_p.first.second,(slope)R_p.second);
            }
        }
        delete R_prime;
    }
    return R;
}

comparison Sloped_relation::compare(const Sloped_relation& other){

    // Remember: undefined < stay < decrease

    /* First of all check whether this < other.
     * this < other means both this <= other and this != other
     * this <= other means that this(h1, h2) <= other(h1, h2) for all h1, h2
     * So we go through each binding (h1, h2) in this->slope_map
     * (i.e. for which this(h1, h2) > undefined) and check that a corresponding
     * binding exists in other (i.e. other(h1, h2) > undefined), and also that
     * then this(h1, h2) <= other(h1, h2).
     * Note that for the bindings not in the map, we immediately have that
     *      this(h1, h2) = undefined <= other(h1, h2)
     */
    bool all_leq = true;
    bool found_lt = false;
    for(auto my_binding : *(this->slope_map)) {
        auto other_binding = (other.slope_map)->find(my_binding.first);
        if (other_binding == (other.slope_map)->end()
              || other_binding->second < my_binding.second) {
            all_leq = false;
            break;
        }
        if (my_binding.second < other_binding->second) {
            found_lt = true;
        }
    }

    // Now, if all_leq is true then we know this <= other
    if (all_leq) {
        /* If all the bindings in this have a corresponding equal value in
         * other (i.e. when found_lt is false), then this = other iff there are
         * the same number of bindings in both slope maps
         */
        if (!found_lt && this->size() == other.size()) {
            return eq;
        /* Otherwise we have this != other, and so this < other */
        } else {
            return lt;
        }
    }

    /*
     * Now, we know that this != other and so we have to determine whether
     * other <= this (in which case, since other != this, we have this > other).
     * If this is not the case, then the only other possibility is that they are
     * incomparable.
     */
    for(auto other_binding : *(other.slope_map)) {
        auto my_binding = (this->slope_map)->find(other_binding.first);
        if (my_binding == (this->slope_map)->end()
              || my_binding->second < other_binding.second) {
            return noncomp;
        }
    }

    return gt;
}


//==================================================================
// USE : SCC

void fill_order(int v, bool* visited, std::stack<int>* s, slope** g, int n){
    visited[v] = true;
    for(int i = 0; i < n; ++i)
        if(g[v][i] != Undef)
            if(!visited[i])
                fill_order(i, visited, s, g, n);
    s->push(v);
}

bool find_scc(int v, bool* visited, slope** g, int n){
    visited[v] = true;
    bool found = false;
    for (int i = 0 ; i < n; ++i) {
        if(g[i][v] != Undef) {
            if (!visited[i]) {
                if(g[i][v] == Downward) {
                    return true;
                }
                if (find_scc(i, visited, g, n)) {
                    found = true;
                    break;
                }
            }
        }
    }
    return found;
}

bool Sloped_relation::has_downward_SCC(void){
    int num_heights =
        (num_src_heights > num_dst_heights) ? num_src_heights : num_dst_heights;
    slope** g = (slope**)malloc( num_heights * sizeof(slope*));
    bool* visited = (bool*)malloc(num_heights * sizeof(bool));
    for( int i = 0 ; i < num_heights; i++ ){
        g[i] = (slope*)malloc(num_heights * sizeof(slope));
        visited[i] = false;
        for( int j = 0 ; j < num_heights ; j++ ){
            g[i][j] = Undef;
        }
    }

    for( Pair<Int_pair,int> pair : *(this->slope_map) ){
        g[(pair.first).first][(pair.first).second] = static_cast<slope>(pair.second);
    }
    std::stack<int> s;
    for(int i = 0; i < num_heights; i++)
        if(!visited[i])
            fill_order(i, visited, &s, g, num_heights);

    for( int i = 0 ; i < num_heights; i++) {
        visited[i] = false;
    }
    bool found = false;
    while (!s.empty() && !found){
        int v = s.top();
        s.pop();
        if (!visited[v]){
            if (g[v][v] == Downward) {
                found = true;
            } else {
                found = find_scc(v, visited, g, num_heights);
            }
        }
    }
    
   
    for( int i = 0 ; i < num_heights ; i++ ){
        free(g[i]);
    }
    free(g);
    free(visited);

    return found;
}

//==================================================================
// MISC

std::ostream& operator<<(std::ostream& os, const Sloped_relation& r) {
    os << "--------------------------------------------" << std::endl;
    os << "Size of relation: " << r.size() << std::endl;
    os << "--------------------------------------------" << std::endl;
    for( auto binding : *(r.slope_map) ){
        os << "\tsource height: " << binding.first.first;;
        os << ", dest height: " << binding.first.second;
        os << ", slope:" << binding.second;
        os << std::endl;
    }
    os << "============================================" << std::endl;
    return os;
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

bool operator< (const Sloped_relation& R, const Sloped_relation& L){
    return (R.slope_map < L.slope_map);
}

void Sloped_relation::print_(void){
    std::cout << *this;
}

// Returns true iff lhs < rhs
bool compare_slope(int lhs, int rhs) {
    if (lhs == Undef && rhs != Undef) {
        return true;
    } else if (lhs == Stay && rhs == Downward) {
        return true;
    } else {
        return false;
    }
}

bool Sloped_relation::linear_order::operator() (const Sloped_relation& lhs, const Sloped_relation& rhs) const {

    int max_lhs = 
        lhs.num_src_heights > lhs.num_dst_heights ?
            lhs.num_src_heights : lhs.num_dst_heights;
    int max_rhs = 
        rhs.num_src_heights > rhs.num_dst_heights ?
            rhs.num_src_heights : rhs.num_dst_heights;
    int max = max_lhs > max_rhs ? max_lhs : max_rhs;

    for (int boundary = 0; boundary < max; boundary++) {
        // First compare the column of the boundary (minus apex)
        for (int row = 0; row < boundary; row++) {
            // Calculate the slope of the left-hand relation at this point
            int lhs_slope = Undef;
            if (row < lhs.num_src_heights && boundary < lhs.num_dst_heights) {
                auto entry = lhs.slope_map->find(Int_pair(row, boundary));
                if (entry != lhs.slope_map->end()) {
                    lhs_slope = entry->second;
                }
            }
            // Calculate the slope of the right-hand relation at this point
            int rhs_slope = Undef;
            if (row < rhs.num_src_heights && boundary < rhs.num_dst_heights) {
                auto entry = rhs.slope_map->find(Int_pair(row, boundary));
                if (entry != rhs.slope_map->end()) {
                    rhs_slope = entry->second;
                }
            }
            // Now compare slopes
            if (lhs_slope == rhs_slope) {
                continue;
            } else {
                return compare_slope(lhs_slope, rhs_slope);
            }
        }
        // Second compare the row of the boundary (including the apex)
        for (int col = 0; col <= boundary; col++) {
            // Calculate the slope of the left-hand relation at this point
            int lhs_slope = Undef;
            if (boundary < lhs.num_src_heights && col < lhs.num_dst_heights) {
                auto entry = lhs.slope_map->find(Int_pair(boundary, col));
                if (entry != lhs.slope_map->end()) {
                    lhs_slope = entry->second;
                }
            }
            // Calculate the slope of the right-hand relation at this point
            int rhs_slope = Undef;
            if (boundary < rhs.num_src_heights && col < rhs.num_dst_heights) {
                auto entry = rhs.slope_map->find(Int_pair(boundary, col));
                if (entry != rhs.slope_map->end()) {
                    rhs_slope = entry->second;
                }
            }
            // Now compare slopes
            if (lhs_slope == rhs_slope) {
                continue;
            } else {
                return compare_slope(lhs_slope, rhs_slope);
            }
        }
    }

    // If we get here, then the two relations are equal in this ordering
    // So it's not the case that lhs < rhs
    return false;
}
