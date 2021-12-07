#include "heighted_graph.hpp"
#include "sloped_relation.hpp"
#include "types.c"

#include <cassert>
#include <chrono>
#include <iostream>
#include <fstream>
#include <set>
#include <thread>
#include <utility>

// N.B. These MUST match the corresponding constants in the OCaml code
//      Look in soundcheck.ml
const int Heighted_graph::FAIL_FAST       = 0b0001;
const int Heighted_graph::USE_SCC_CHECK   = 0b0010;
const int Heighted_graph::USE_IDEMPOTENCE = 0b0100;
const int Heighted_graph::USE_MINIMALITY  = 0b1000;

int Heighted_graph::parse_flags(const std::string flags_s) {
    int flags = 0;
    for (char c : flags_s) {
        switch (c) {
            case 'f': flags |= FAIL_FAST; break;
            case 's': flags |= USE_SCC_CHECK; break;
            case 'i': flags |= USE_IDEMPOTENCE; break;
            case 'm': flags |= USE_MINIMALITY; break;
        }
    }
    return flags;
}

// Construcor
Heighted_graph::Heighted_graph(int max_nodes) {

    assert(max_nodes >= 0);

    this->max_nodes = max_nodes;
    number_of_edges = 0;

    ccl_initial_size = 0;
    ccl_size = 0;
    ccl_replacements = 0;
    ccl_rejections = 0;
    compositions = 0;
    comparisons = 0;
    loop_checks = 0;
    checked_size_sum = 0;

    compose_time = std::chrono::duration<double, std::milli>::zero();
    compare_time = std::chrono::duration<double, std::milli>::zero();
    insertion_time = std::chrono::duration<double, std::milli>::zero();
    loop_check_time = std::chrono::duration<double, std::milli>::zero();

    rejected = new Relation_LIST();

    h_change_ =
        (Sloped_relation***) malloc(sizeof(Sloped_relation**) * max_nodes);
    Ccl =
        (Sloped_Relation_SET***) malloc(sizeof(Sloped_Relation_SET**) * max_nodes);

    for( int i = 0 ; i < max_nodes ; i++ ){
        Ccl[i] = 
            (Sloped_Relation_SET**) malloc(sizeof(Sloped_Relation_SET*) * max_nodes);
        h_change_[i] =
            (Sloped_relation**) malloc(sizeof(Sloped_relation*) * max_nodes);
        for( int j = 0 ; j < max_nodes ; j++ ){
            h_change_[i][j] = 0;
            Ccl[i][j] = new Sloped_Relation_SET();
        }
    }

}

void clean_up(Sloped_Relation_SET*** ccl,
              Sloped_relation*** h_change_,
              int num_nodes,
              Relation_LIST* rejected) {

    for( int source = 0 ; source < num_nodes ; source++ ){
        for( int sink = 0 ; sink < num_nodes ; sink++ ){
            for( Sloped_relation* s : *(ccl[source][sink]) ){
                delete s;
            }
            delete ccl[source][sink];
        }
        delete h_change_[source];
        delete ccl[source];
    }
    delete ccl;
    delete h_change_;

    for (Sloped_relation* R : *rejected){
        delete R;
    }
    delete rejected;

}

// Destructor
Heighted_graph::~Heighted_graph(void) {
    for(Int_SET* heights : HeightsOf){
        delete heights;
    }
    std::thread t(clean_up, Ccl, h_change_, max_nodes, rejected);
    t.detach();
}

// Methods for constructing the height graph
void Heighted_graph::add_node(int n) {
    if (node_idxs.find(n) == node_idxs.end()) {
        int next_idx = node_idxs.size();
        assert(next_idx < max_nodes);
        node_idxs[n] = next_idx;
        HeightsOf.push_back(new Int_SET());
    }
}

void Heighted_graph::add_height(int n, int h) {
    add_node(n);
    int idx = node_idxs.at(n);
    HeightsOf[idx]->insert(h);
}

void Heighted_graph::add_edge(int source, int sink) {
    add_node(source);
    add_node(sink);
    int src_idx = node_idxs[source];
    int sink_idx = node_idxs[sink];
    if ( h_change_[src_idx][sink_idx] == 0 ) {
        number_of_edges++;
        Sloped_relation* R = new Sloped_relation(new Map<int,Int_pair_SET*>(),new Map<int,Int_pair_SET*>(),new Map<Int_pair,int>());
        h_change_[src_idx][sink_idx] = R;
        Ccl[src_idx][sink_idx]->insert(R);
        ccl_initial_size++;
        ccl_size++;
    }
}

void Heighted_graph::add_hchange(int source, int source_h, int sink, int sink_h, slope s) {
    add_edge(source, sink);
    int src_idx = node_idxs[source];
    int sink_idx = node_idxs[sink];
    h_change_[src_idx][sink_idx]->add(source_h, sink_h, s);
}

void Heighted_graph::add_stay(int source_node, int source_h, int sink_node, int sink_h) {
    add_hchange(source_node, source_h, sink_node, sink_h, Stay);
}

void Heighted_graph::add_decrease(int source_node, int source_h, int sink_node, int sink_h) {
    add_hchange(source_node, source_h, sink_node, sink_h, Downward);
}

int Heighted_graph::num_nodes(void) {
    return node_idxs.size();
}

int Heighted_graph::num_edges(void) {
    return number_of_edges;
}

bool Heighted_graph::check_self_loop(Sloped_relation* R, int node, int opts) {

    auto start = std::chrono::system_clock::now();
    loop_checks++;
    checked_size_sum += R->size();

    bool result = false;

    if ((opts & USE_SCC_CHECK) != 0) {
        result = R->has_downward_SCC();
    } else {
        // Compute R composed with R if using the idempotent method
        // or the transitive closure otherwise (i.e. using the standard method)
        Sloped_relation* R2 = 
            ((opts & USE_IDEMPOTENCE) != 0)
                ? R->compose(*R)
                : R->compute_transitive_closure();

        // If we're using the idempotent method and the relation is not
        // idempotent then trivially return true
        if (((opts & USE_IDEMPOTENCE) != 0) && !(*R2 == *R)) {
            result = true;
            loop_checks--;
            checked_size_sum -= R->size();
        } else {
            // Otherwise, check we have a self-loop in the relevant relation
            Map<Int_pair,int>* slopes = R2->get_slopes();

            if (HeightsOf[node]->size() < slopes->size()) {
                // Iterate over all heights h in the node, to see if (h, h) is
                // mapped to the Downward slope.
                for( int h : *(HeightsOf.at(node)) ){
                    auto exists = slopes->find(Int_pair(h,h));
                    if( exists != slopes->end() && exists->second == Downward ){
                        result = true;
                        break;
                    }
                }
            } else {
                // Only iterate over all entries in the transitive closure of R
                // if this will be quicker then going over the heights
                for(auto it = slopes->begin(); it != slopes->end(); it++) {
                    Int_pair heights = it->first;
                    if (heights.first == heights.second && it->second == Downward) {
                        result = true;
                        break;
                    }
                }
            }
        }
    }

    auto end = std::chrono::system_clock::now();
    loop_check_time += end - start;

    return result;
}

bool Heighted_graph::check_Ccl(int opts) {
    int num_nodes = this->num_nodes();
    for( int node = 0; node < num_nodes; node++ ){
        Sloped_Relation_SET* Ccl_nd = Ccl[node][node];
        for( Sloped_relation* R : *Ccl_nd ){
            if(!check_self_loop(R, node, opts)) {
                return false;
            }
        }
    }
    return true;
}

bool Heighted_graph::check_soundness(int opts){

    // if ((opts & FAIL_FAST) != 0) std::cout << "Fail Fast\n";
    // if ((opts & USE_SCC_CHECK) != 0) std::cout << "Use SCC Check\n";
    // if ((opts & USE_IDEMPOTENCE) != 0) std::cout << "Use Idempotence\n";
    // if ((opts & USE_MINIMALITY) != 0) std::cout << "Use Minimality\n";

    // We cannot combine the idempotence and minimality optimisations.
    assert(((opts & USE_IDEMPOTENCE) == 0) || ((opts & USE_MINIMALITY) == 0));
    // It doesn't make sense to combine the idempotence and the SCC-based loop check
    assert(((opts & USE_IDEMPOTENCE) == 0) || ((opts & USE_SCC_CHECK) == 0));

    /* N.B. It is useless to combine the fast-fail and minimality optimisations.
            The point of the minimality optimisation is to not have to check for
            self-loops in all sloped relations. But when applying the minimality
            optimisation, it can be that we replace a sloped relation with a
            self-loop by one without a self-loop. Therefore, if checking for
            self-loops on-the-fly, we would still end up having to check every
            sloped relation anyway.

            If both flags are set, then we ignore (unset) fast-fail
     */
    if ((opts & USE_MINIMALITY) != 0) {
        opts &= ~FAIL_FAST;
    }

    // If fail-fast, then need to check initial sloped relations for self-loops
    if ((opts & FAIL_FAST) != 0) {
        if (!check_Ccl(opts)) {
            return false;
        }
    }

    std::chrono::time_point<std::chrono::system_clock> start;
    std::chrono::time_point<std::chrono::system_clock> end;

    Relation_LIST* rejected = new Relation_LIST();

    // Now compute the CCL
    bool done = false;
    int num_nodes = this->num_nodes();
    while( !done ){
        // reset loop flag
        done = true;
        for( int source = 0 ; source < num_nodes ; source++ ){
        for( int sink = 0 ; sink < num_nodes ; sink++ ){
        for( int middle = 0 ; middle < num_nodes ; middle++ ){

            for( Sloped_relation* P : *Ccl[source][middle] ){
                if( P->size() == 0 ) continue;
                for( Sloped_relation* Q : *Ccl[middle][sink] ){
                    if( Q->size() == 0 ) continue;
                    start = std::chrono::system_clock::now();
                    Sloped_relation* R = P->compose(*Q);
                    end = std::chrono::system_clock::now();
                    compose_time += (end - start);
                    compositions++;
                    if( R->size() == 0 ) continue;
                    done = true;

                    bool need_to_add = true;
                    for( Sloped_relation* S : *Ccl[source][sink] ){
                        comparisons++;
                        if ((opts & USE_MINIMALITY) != 0) {
                            start = std::chrono::system_clock::now();
                            comparison result = R->compare(*S);
                            end = std::chrono::system_clock::now();
                            compare_time += (end - start);
                            if( result == lt ){
                                ccl_replacements++;
                                ccl_size--;
                                (Ccl[source][sink])->erase(S);
                                rejected->push_back(S);
                                break;
                            }
                            else if( result == eq || result == gt ){
                                ccl_rejections++;
                                need_to_add = false;
                                break;
                            }
                        } else {
                            start = std::chrono::system_clock::now();
                            bool equal = (*S == *R);
                            end = std::chrono::system_clock::now();
                            compare_time += (end - start);
                            if( equal ){
                                ccl_rejections++;
                                need_to_add = false;
                                break;
                            }
                        }
                    }

                    // If fail-fast, then check for self-loop if necessary
                    bool fail_now = false;
                    if (need_to_add 
                            && ((opts & FAIL_FAST) != 0)
                            && source == sink
                            && !(check_self_loop(R, source, opts))) {
                        fail_now = true;
                        need_to_add = false;
                    }

                    if( need_to_add ){
                        // Otherwise, not done with the fixed point computation
                        done = false;
                        // So add the newly computed sloped relation
                        start = std::chrono::system_clock::now();
                        (Ccl[source][sink])->insert(R);
                        end = std::chrono::system_clock::now();
                        insertion_time += (end - start);
                        ccl_size++;
                    } else {
                        rejected->push_back(R);                        
                    }

                    if (fail_now) { return false; }
                }
            }
        }
        }
        }
    }

    // If not using fast-fail, then check for self-loops in the computed CCL
    if ((opts & FAIL_FAST) == 0) {
        if (!check_Ccl(opts)) {
            return false;
        }
    }

    return true;
}

void Heighted_graph::print_Ccl(void){
    int num_nodes = this->num_nodes();
    for( int source = 0 ; source < num_nodes ; source++ ){
    for( int sink = 0 ; sink < num_nodes  ; sink++ ){
        for( auto S : *Ccl[source][sink]){
            std::cout << "<><><><><><><><><><><><><><><><><><><><><><><><><><><><><>" << std::endl;
            std::cout << source << " " << sink << std::endl;
            S->print_();
            std::cout << "<><><><><><><><><><><><><><><><><><><><><><><><><><><><><>" << std::endl;
        }
    }
    }
}

void Heighted_graph::print_statistics(void) {
    std::cout << "Initial CCL size: " << ccl_initial_size << std::endl;
    std::cout << "Final CCL size: " << ccl_size << std::endl;
    std::cout << "CCL Rejections: " << ccl_rejections << std::endl;
    std::cout << "CCL Replacements: " << ccl_replacements << std::endl;
    std::cout << "Sloped relations computed: " << compositions << std::endl;
    std::cout << "Time spent computing sloped relations (ms): "
              << compose_time.count() << std::endl;
    std::cout << "Sloped relations compared: " << comparisons << std::endl;
    std::cout << "Time spent comparing sloped relations (ms): "
              << compare_time.count() << std::endl;
    std::cout << "Time spent inserting sloped relations (ms): "
              << insertion_time.count() << std::endl;
    std::cout << "Number of self-loop checks: " << loop_checks << std::endl;
    std::cout << "Time spent loop checking (ms): "
              << loop_check_time.count() << std::endl;
    std::cout << "Average size of loop-checked sloped relations: "
              << (checked_size_sum / loop_checks) << std::endl;
}
