#include <cassert>
#include <iostream>
#include <map>
#include <ostream>
#include <set>
#include <stack>

#include "sloped_relation.hpp"
#include "types.c"


//==================================================================
// USE : ON CREATION
Sloped_relation::Sloped_relation(
        int num_src_heights,
        int num_dst_heights,
        slope diagonal)
{

    this->num_src_heights = num_src_heights;
    this->num_dst_heights = num_dst_heights;

    repr_matrix = (int**) malloc (sizeof(int*) * num_src_heights);

    for( int i = 0 ; i < num_src_heights ; i++ ){
        repr_matrix[i] = (int*) malloc(sizeof(int) * num_dst_heights);
        for( int j = 0 ; j < num_dst_heights ; j++ ){
            if (i == j) {
                repr_matrix[i][j] = diagonal;
            } else  {
                repr_matrix[i][j] = Undef;
            }
        }
    }

}

Sloped_relation::Sloped_relation(const Sloped_relation& R) {

    this->num_src_heights = R.num_src_heights;
    this->num_dst_heights = R.num_dst_heights;

    repr_matrix = (int**) malloc(sizeof(int*) * num_src_heights);

    for( int i = 0 ; i < num_src_heights ; i++ ){
        repr_matrix[i] = (int*) malloc(sizeof(int) * num_dst_heights);
        for( int j = 0 ; j < num_dst_heights ; j++ ){
            repr_matrix[i][j] = R.repr_matrix[i][j];
        }
    }

    if (R.slope_map != NULL) {
        this->forward_map = new Map<int,Int_pair_SET*>();
        this->backward_map = new Map<int,Int_pair_SET*>();
        this->slope_map = new Map<Int_pair,int>();
        for( auto pair : *(R.forward_map) ){
            (this->forward_map)->insert(
                    Pair<int,Int_pair_SET*>(
                        pair.first,
                        new Int_pair_SET(*(pair.second))
                    )
                );
        }
        for( auto pair : *(R.backward_map) ){
            this->backward_map->insert(
                    Pair<int,Int_pair_SET*>(
                        pair.first,
                        new Int_pair_SET(*(pair.second))
                    )
                );
        }
        for( auto pair : *(R.slope_map) ){
            this->slope_map->insert(pair);
        }
    }

    if (R.has_down_scc != NULL) {
        this->has_down_scc = (bool*) malloc(sizeof(bool));
        *(this->has_down_scc) = *(R.has_down_scc);
    }

}

//==================================================================
// USE : ON DISTRUCTION
Sloped_relation::~Sloped_relation(void) {

    // There is always a representation matrix, since this is allocated by the
    // only (public) constructor
    for (int i = 0; i < num_src_heights; i++) {
        free(repr_matrix[i]);
    }
    free(repr_matrix);

    if (forward_map != NULL) {
        for (auto pair : *(forward_map)) {
            if (pair.second) delete pair.second;
        }
        delete forward_map;
    }
    if (backward_map != NULL) {
        for (auto pair : *(backward_map)) {
            if (pair.second) delete pair.second;
        }
        delete backward_map;
    }
    if (slope_map != NULL) {
        delete slope_map;
    }

    if (has_down_scc) {
        free(has_down_scc);
    }

}


void Sloped_relation::reset(void){

    // Reset the matrix representation

    for (int src = 0; src < num_src_heights; src++) {
    for (int dst = 0; dst < num_dst_heights; dst++) {
        repr_matrix[src][dst] = Undef;
    }
    }

    // Reset the slope maps

    if (slope_map != NULL) {
        slope_map->clear();
    }

    if (forward_map != NULL) {
        for (auto pair : *(forward_map)) {
            if (pair.second) delete pair.second;
        }
        forward_map->clear();
    }

    if (backward_map != NULL) {
        for (auto pair : *(backward_map)) {
            if (pair.second) delete pair.second;
        }
        backward_map->clear();
    }

    // Remove result
    if (has_down_scc != NULL) {
        free(has_down_scc);
    }

}

//==================================================================
// USE : GET AND SET
void Sloped_relation::initialize(void){

    std::lock_guard lock(this->initialize_mutex);

    // Don't do anything if already initialised
    if (slope_map != NULL) {
        return;
    }

    // Initialise maps
    this->forward_map = new Map<int,Int_pair_SET*>();
    this->backward_map = new Map<int,Int_pair_SET*>();
    this->slope_map = new Map<Int_pair,int>();

    // Now populate maps from the matrix representation
    for (int i = 0; i < num_src_heights; i++) {
    for (int j = 0; j < num_dst_heights; j++) {
        if (repr_matrix[i][j] != Undef) {

            // Make sure mapping for the source height exists in the forward map
            auto exists = forward_map->find(i);
            if (exists == forward_map->end()) {
                forward_map->insert(
                        Pair<int,Int_pair_SET*>(i, new Int_pair_SET())
                    );
            }
            // Add slope to forward map
            (forward_map->at(i))->insert(Int_pair(j, (int) repr_matrix[i][j]));

            // Make sure mapping for the destination height exists in the
            // backward map
            exists = backward_map->find(j);
            if (exists == backward_map->end()) {
                backward_map->insert(
                        Pair<int,Int_pair_SET*>(j, new Int_pair_SET())
                    );
            }
            // Add slope to backward map
            (backward_map->at(j))->insert(Int_pair(i, (int) repr_matrix[i][j]));

            // Add mapping to the slope map
            slope_map->insert(
                    Pair<Int_pair,int>(Int_pair(i, j), (int) repr_matrix[i][j])
                );

        }
    }
    }

}

bool Sloped_relation::add(int h1, int h2, slope s) {

    // This method will only monotonically increase the relation

    assert (h1 < num_src_heights);
    assert (h2 < num_dst_heights);

    // Update the matrix representation
    if (repr_matrix[h1][h2] < s) {
        repr_matrix[h1][h2] = s;
    } else {
        return false;
    }

    // If the maps have been initialised, update them too
    if (slope_map != NULL) {
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
            backward_map->insert(
                    Pair<int,Int_pair_SET*>(h2,new Int_pair_SET())
                );
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

        Int_pair p3(h1, h2);
        auto exists_s = slope_map->find(p3);
        if( exists_s == slope_map->end() ){
            slope_map->insert(Pair<Int_pair,int>(p3, s));
        }
        // Don't overwrite an existing Downward mapping with a Stay
        else if( s == Downward || slope_map->at(p3) != Downward ){
            slope_map->at(p3) = s;
        }
    }

    // New edge in the relation may invalidate negative status of the relation
    // having a downward SCC, but not a positive status
    if (this->has_down_scc != NULL && !(*(this->has_down_scc))) {
        free(this->has_down_scc);
    }

    return true;
}

void Sloped_relation::remove(int h1, int h2, slope s) {
    assert (h1 < num_src_heights);
    assert (h2 < num_dst_heights);

    // Update the matrix representation
    repr_matrix[h1][h2] = slope::Undef;

    // If the maps have been initialised, update them too
    if (slope_map != NULL) {
        // Remove forward mapping
        auto exists_h1 = forward_map->find(h1);
        if( exists_h1 != forward_map->end() ){
            forward_map->at(h1)->erase(Int_pair(h2, s));
            if(forward_map->at(h1)->size() == 0) {
                forward_map->erase(h1);
            }
        }

        // Add backward mapping
        auto exists_h2 = backward_map->find(h2);
        if( exists_h2 != backward_map->end() ){
            backward_map->at(h2)->erase(Int_pair(h1, s));
            if(backward_map->at(h2)->size() == 0) {
                backward_map->erase(h2);
            }
        }

        Int_pair p3(h1, h2);
        auto exists_s = slope_map->find(p3);
        if( exists_s != slope_map->end() ){
            slope_map->erase(p3);
        }
    }

    // New edge in the relation may invalidate negative status of the relation
    // having a downward SCC, but not a positive status
    if (this->has_down_scc != NULL && !(*(this->has_down_scc))) {
        free(this->has_down_scc);
    }


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

const Map<int,Int_pair_SET*>* Sloped_relation::get_forward_map() {
    return this->forward_map;
}

const Map<Int_pair,int>* Sloped_relation::get_slope_map() {
    return this->slope_map;
}

Int_pair_SET* Sloped_relation::get_height_neighbours(int src_height) {
    try {
        return this->forward_map->at(src_height);
    } catch (std::out_of_range) {
        return NULL;
    }
}

//==================================================================
// USE : USEFUL COMPUTATION METHODS

void Sloped_relation::compose(Sloped_relation& left, Sloped_relation& right) {

    // The receiver must be big enough to store the result
    assert (this->num_src_heights >= left.num_src_heights);
    assert (this->num_dst_heights >= right.num_dst_heights);

    // The arguments must be initialised
    assert (left.slope_map != NULL && right.slope_map != NULL);

    // Now compute the composition
    for (auto p1 : *(left.forward_map)) {
        for (auto p2 : *(right.backward_map)) {

            int h1 = p1.first;
            int h3 = p2.first;

            Int_pair_SET* small_slope_set;
            Int_pair_SET* large_slope_set;

            if (p1.second->size() > p2.second->size()) {
                small_slope_set = p2.second;
                large_slope_set = p1.second;
            } else {
                small_slope_set = p1.second;
                large_slope_set = p2.second;
            }

            slope s = Stay;
            bool found = false;

            for (Int_pair p3 : *small_slope_set) {
                Int_pair p3_down(p3.first, Downward);
                Int_pair p3_stay(p3.first, Stay);
                // If there is a downslope from the intermediate height to the
                // destination, then there is a down slope connecting source and
                // destination, no matter what the slope from the source to the
                // intermediate height is
                auto exists = large_slope_set->find(p3_down);
                if (exists != large_slope_set->end()) {
                    s = Downward;
                }
                // If there is no slope connecting the intermediate height to
                // the destination, we keep on looking for a connecting
                // intermediate height
                else if (large_slope_set->find(p3_stay) == large_slope_set->end()) {
                    continue;
                }
                // Otherwise, there is a stay slope connecting the intermediate
                // height to the destination, so the slope connecting the source
                // to the destination is whatever slope connects the source to
                // the intermediate height
                else {
                    s = (slope) p3.second;
                }
                // We record that we've found an intermediate height
                found = true;
                // And if the result slope is down, we can stop looking
                // immediately, since this is the maximum slope we would ever
                // compute; on the other hand, if the resulting slope is stay,
                // then we need to check other intermediate heights, as these
                // may result in a down slope
                if (s == Downward) {
                    break;
                }
            }

            // If there was an intermediate height connecting h1 and h3, then
            // add the element to the relation
            if (found) {
                this->add(h1, h3, s);
            }

        }
    }

}

Sloped_relation* Sloped_relation::compose(Sloped_relation& other){

    // Create an empty sloped relation for the result
    Sloped_relation* result =
        new Sloped_relation(this->num_src_heights, other.num_dst_heights);

    // Compute the composition in-place in the new relation
    result->compose(*this, other);

    // Initialise the result
    result->initialize();

    return result;

}

Sloped_relation* Sloped_relation::compute_transitive_closure(void){

    // We will only call this method on initialised relations
    assert (slope_map != NULL);

    bool cont;
    Sloped_relation* result = new Sloped_relation(*this);
    do {
        // Reset flag
        cont = false;
        // Compute composition of this relation and the old result
        Sloped_relation* composition = this->compose(*result);
        // Now add in composition to the old result (i.e. compute the union)
        // We record if any of the mappings in the composition were new,
        // in which case we need to iterate again (since then the new result is
        // not a prefixed point)
        for (auto it = composition->slope_map->begin();
             it != composition->slope_map->end();
             it++)
        {
            int src = (it->first).first;
            int dst = (it->first).second;
            slope s = (slope) (it->second);
            cont |= result->add(src, dst, s);
        }
        delete composition;
    } while (cont);
    return result;
}

comparison Sloped_relation::compare(const Sloped_relation& other){

    // We will only call this method on initialised relations
    assert (slope_map != NULL);

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

bool Sloped_relation::has_self_loop(void) {

    // We will only call this method on initialised relations
    assert (slope_map != NULL);

    // Only iterate over entries in the slope map if this will be quicker than
    // going over the heights directly
    if (this->slope_map->size() < this->num_src_heights) {
        for(auto it = this->slope_map->begin(); it != this->slope_map->end(); it++) {
            Int_pair heights = it->first;
            if (heights.first == heights.second && it->second == Downward) {
                return true;
            }
        }
    } else {
        // Iterate over all heights h in the node, to see if (h, h) is
        // mapped to the Downward slope.
        for (int h = 0; h < num_src_heights; h++){
            auto exists = this->slope_map->find(Int_pair(h,h));
            if( exists != this->slope_map->end() && exists->second == Downward ){
                return true;
            }
        }
    }
    return false;

}

//==================================================================
// USE : SCC

/**
 * Tarjan's algorithm for finding strongly-connected components.
 * Reference: https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
 * Slightly modified to return true if we find an edge inside a
 * strongly-connected component with a Downward slope.
 */
bool find_scc
    (
        int n,
        std::stack<int>& s,
        bool* on_stack,
        int* idxs,
        int* low_links,
        int& next_idx,
        int** repr_matrix,
        int num_src_heights,
        int num_dst_heights
    )
{

    idxs[n] = next_idx++;
    low_links[n] = idxs[n];

    // Only need to push this node to the stack and process it if it might have
    // edges coming out of it
    if (n < num_src_heights) {
        
        on_stack[n] = true;
        s.push(n);

        // Consider successors of n
        for (int m = 0; m < num_dst_heights; m++) {
            slope slp = (slope) repr_matrix[n][m];
            if (slp == Undef) {
                continue;
            }
            // If successor m has not been visited yet
            if (idxs[m] == -1) {
                // Recurse on it
                bool result = 
                    find_scc(m, s, on_stack, idxs, low_links, next_idx,
                             repr_matrix, num_src_heights, num_dst_heights);
                if (result) {
                    return true;
                }
                low_links[n] = 
                    low_links[n] < low_links[m] ? low_links[n] : low_links[m];
            }
            // Otherwise, if successor is in the stack then it is in the current
            // strongly-connected component
            else if (on_stack[m]) {
                // If the edge has a downward slope, then we can return true
                // immediately
                if (slp == Downward) {
                    return true;
                }
                // Otherwise, update low_link for this height, as per original
                // algorithm
                else {
                    low_links[n] = 
                        low_links[n] < idxs[m] ? low_links[n] : idxs[m];
                }
            }
            // Otherwise successor is in an already processed strongly-connected
            // component
            else {
                // Do nothing
            }
        }

        // If the height n is the root of a discovered strongly-connected
        // component then remove that component from the stack
        if (low_links[n] == idxs[n]) {
            int m = -1;
            while (m != n) {
                m = s.top();
                s.pop();
                on_stack[m] = false;
            }
        }
    }

    // We did not find a downward slope in this strongly-connected component
    return false;

}

bool Sloped_relation::has_downward_SCC(void) {

    if (this->has_down_scc != NULL) {
        return *(this->has_down_scc);
    }

    // Allocate new boolean to store result and initialise
    this->has_down_scc = (bool*) malloc(sizeof(bool));
    *(this->has_down_scc) = false;

    int num_heights =
        (num_src_heights > num_dst_heights) ? num_src_heights : num_dst_heights;
    
    bool* on_stack = (bool*) malloc(num_heights * sizeof(bool));
    int*  idxs = (int*) malloc(num_heights * sizeof(int));
    int*  low_links = (int*) malloc(num_heights * sizeof(int));
    
    for (int i = 0 ; i < num_heights; i++) {
        on_stack[i] = false;
        idxs[i] = -1;
        low_links[i] = -1;
    }

    std::stack<int> s;

    bool found = false;
    int next_idx = 0;
    for (int n = 0; n < num_heights; n++) {
        if (idxs[n] == -1
                && find_scc(n, s, on_stack, idxs, low_links, next_idx,
                            repr_matrix, num_src_heights, num_dst_heights))
        {
            *(this->has_down_scc) = true;
            break;
        }
    }

    free(on_stack);
    free(idxs);
    free(low_links);

    return *(this->has_down_scc);
}

bool Sloped_relation::has_downward_slope() {
    this->initialize();
    for (auto slope : *(this->slope_map)){
        if (slope.second == Downward){
            return true;
        }
    }
    return false;
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
    // We will only call this method on initialised relations
    assert (L.slope_map != NULL && R.slope_map != NULL);
    // Shortcut: if the maps have different sizes, they can't be equal
    if( (R.slope_map)->size() != (L.slope_map)->size() ){
        return false;
    }
    // Otherwise, we can iterate through the map entries in lockstep
    auto left_it = L.slope_map->begin();
    auto right_it = R.slope_map->begin();
    while (left_it != L.slope_map->end() && right_it != R.slope_map->end()) {
        if (left_it->first != right_it->first)
            return false;
        if (left_it->second != right_it->second)
            return false;
        left_it++;
        right_it++;
    }
    // If we get to the end of both at the same time, the maps are equal
    return left_it == L.slope_map->end() && right_it == R.slope_map->end();
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
            int lhs_slope =
                (row < lhs.num_src_heights && boundary < lhs.num_dst_heights) ?
                      lhs.repr_matrix[row][boundary]
                    : Undef;
            // Calculate the slope of the right-hand relation at this point
            int rhs_slope =
                (row < rhs.num_src_heights && boundary < rhs.num_dst_heights) ?
                      rhs.repr_matrix[row][boundary]
                    : Undef;
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
            int lhs_slope =
                (boundary < lhs.num_src_heights && col < lhs.num_dst_heights) ?
                      lhs.repr_matrix[boundary][col]
                    : Undef;
            // Calculate the slope of the right-hand relation at this point
            int rhs_slope =
                (boundary < rhs.num_src_heights && col < rhs.num_dst_heights) ?
                      rhs.repr_matrix[boundary][col]
                    : Undef;
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
