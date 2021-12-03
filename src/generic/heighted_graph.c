#include "heighted_graph.hpp"
#include "sloped_relation.hpp"
#include "types.c"

#include <cassert>
#include <chrono>
#include <iostream>
#include <fstream>
#include <set>
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

// Methods for constructing the height graph
void Heighted_graph::add_node(int n) {
    if (HNode.find(n) == HNode.end()) {
        if( n > max_node ) max_node = n;
        HNode.insert(n);
        HeightsOf.insert(Pair<int,Int_SET*>(n, new Int_SET()));
    }
}

void Heighted_graph::add_height(int n, int h) {
    if (HeightsOf.count(n) == 0) {
        add_node(n);
    }
    (HeightsOf.at(n))->insert(h);
}

void Heighted_graph::add_edge(int source, int sink) {
    if ( h_change_[source][sink] == 0 ) {
        number_of_edges++;
        Sloped_relation* R = new Sloped_relation(new Map<int,Int_pair_SET*>(),new Map<int,Int_pair_SET*>(),new Map<Int_pair,int>());
        h_change_[source][sink] = R;
        (Ccl[source][sink])->insert(R);
        ccl_size++;
    }
}

int Heighted_graph::get_node_size(void){
    return max_node+1;
}

void Heighted_graph::set_node_size(int node_size){
    max_node = node_size;
}

void Heighted_graph::add_hchange(int source, int source_h, int sink, int sink_h, slope s) {
    add_edge(source, sink);
    h_change_[source][sink]->add(source_h, sink_h, s);
}

void Heighted_graph::add_stay(int source_node, int source_h, int sink_node, int sink_h) {
    add_hchange(source_node, source_h, sink_node, sink_h, Stay);
}

void Heighted_graph::add_decrease(int source_node, int source_h, int sink_node, int sink_h) {
    add_hchange(source_node, source_h, sink_node, sink_h, Downward);
}

int Heighted_graph::num_nodes(void) {
    return HNode.size();
}

int Heighted_graph::num_edges(void) {
    return number_of_edges;
}

void Heighted_graph::init(void){

    ccl_size = 0;
    ccl_replacements = 0;
    compositions = 0;
    comparisons = 0;
    loop_checks = 0;
    compose_time = std::chrono::duration<double, std::milli>::zero();
    compare_time = std::chrono::duration<double, std::milli>::zero();
    loop_check_time = std::chrono::duration<double, std::milli>::zero();

    h_change_ = (Sloped_relation***)malloc( sizeof(Sloped_relation**) * (max_node + 1));
    Ccl = (Sloped_Relation_SET***)malloc( sizeof(Sloped_Relation_SET**) * (max_node + 1) );
    for( int i = 0 ; i < (max_node + 1) ; i++ ){
        Ccl[i] = (Sloped_Relation_SET**)malloc( sizeof(Sloped_Relation_SET*) * (max_node + 1) );
        h_change_[i] = (Sloped_relation**)malloc( sizeof(Sloped_relation*) * (max_node + 1));
        for( int j = 0 ; j < (max_node + 1) ; j++ ){
            h_change_[i][j] = 0;
            Ccl[i][j] = new Sloped_Relation_SET();
        }
    }
}

bool Heighted_graph::check_self_loop(Sloped_relation* R, int node, int opts) {


    auto start = std::chrono::system_clock::now();
    loop_checks++;

    bool result = false;

    if ((opts & USE_SCC_CHECK) != 0) {
        result = R->has_downward_SCC();
    } else {
        // Compute R composed with R if using the idempotent method
        // or the transitive closure otherwise (i.e. using the standard method)
        Sloped_relation R2 = 
            ((opts & USE_IDEMPOTENCE) != 0)
                ? *(R->compose(R))
                : R->compute_transitive_closure();

        // If we're using the idempotent method and the relation is not
        // idempotent then trivially return true
        if (((opts & USE_IDEMPOTENCE) != 0) && !(R2 == *R)) {
            result = true;
            loop_checks--;
        } else {
            // Otherwise, check we have a self-loop in the relevant relation
            Map<Int_pair,int>* slopes = R2.get_slopes();

            /* In the worst case we have to iterate over all entries in the
            transitive closure of R (quadratic in the number of heights in the domain),
            but in practice this mightn't often be the case?
            */
            for( Map<Int_pair,int>::iterator it = slopes->begin(); it != slopes->end(); it++ ) {
                Int_pair heights = it->first;
                if (heights.first == heights.second && it->second == Downward) {
                    result = true;
                    break;
                }
            }

            /* The alternative is to iterate over all heights h in the domain, and
            use the find method to see if (h, h) is mapped.
            So this would be h * log(h * h) = h * 2log(h).
            */
            // for( int h : *(HeightsOf.at(node)) ){
            //     auto exists = slopes->find(Int_pair(h,h));
            //     if( exists != slopes->end() && exists->second == Downward ){
            //         result = true;
            //         break;
            //     }
            // }
        }
    }

    auto end = std::chrono::system_clock::now();
    loop_check_time += end - start;

    return result;
}

bool Heighted_graph::check_Ccl(int opts) {
    for( int node : HNode ){
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

    // Now compute the CCL
    bool done = false;
    while( !done ){
        done = true;
        for( int source = 0 ; source < (max_node + 1) ; source++ ){
        for( int middle = 0 ; middle < (max_node + 1) ; middle++ ){
        for( int sink = 0 ; sink < (max_node + 1) ; sink++ ){
            for( Sloped_relation* P : *Ccl[source][middle] ){
                if( P->size() == 0 ) continue;
                for( Sloped_relation* Q : *Ccl[middle][sink] ){
                    if( Q->size() == 0 ) continue;
                    start = std::chrono::system_clock::now();
                    Sloped_relation* R = P->compose(Q);
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
                            if( result == lt ){
                                ccl_replacements++;
                                ccl_size--;
                                (Ccl[source][sink])->erase(S);
                                break;
                            }
                            else if( result == eq || result == gt ){
                                need_to_add = false;
                                break;
                            }
                        } else {
                            start = std::chrono::system_clock::now();
                            bool equal = (*S == *R);
                            end = std::chrono::system_clock::now();
                            if( equal ){
                                need_to_add = false;
                                break;
                            }
                        }
                        compare_time += (end - start);
                    }

                    if( need_to_add ){
                        // If fail-fast, then check for self-loop if necessary
                        if ((opts & FAIL_FAST) != 0) {
                            if( source == sink ){
                                if(! (check_self_loop(R, source, opts))){
                                    delete R;
                                    return false;
                                }
                            }
                        }
                        // Otherwise, not done with the fixed point computation
                        done = false;
                        // So add the newly computed sloped relation
                        (Ccl[source][sink])->insert(R);
                        ccl_size++;
                    } else {
                        delete R;
                    }
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

void Heighted_graph::clean(void){
    for( int source = 0 ; source < (max_node + 1) ; source++ ){
        for( int sink = 0 ; sink < (max_node + 1) ; sink++ ){
            for( Sloped_relation* s : *(Ccl[source][sink]) ){
                delete s;
            }
            delete Ccl[source][sink];
        }
        delete h_change_[source];
        delete Ccl[source];
    }
    delete Ccl;
    delete h_change_;
    for( auto p : HeightsOf){
        delete p.second;
    }
}

void Heighted_graph::print_Ccl(void){
    for( int source = 0 ; source < (max_node + 1) ; source++ ){
    for( int sink = 0 ; sink < (max_node + 1)  ; sink++ ){
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
    std::cout << "Final CCL size: " << ccl_size << std::endl;
    std::cout << "CCL Replacements: " << ccl_replacements << std::endl;
    std::cout << "Sloped relations computed: " << compositions << std::endl;
    std::cout << "Time spent computing sloped relations (ms): "
              << compose_time.count() << std::endl;
    std::cout << "Sloped relations compared: " << comparisons << std::endl;
    std::cout << "Time spent comparing sloped relations (ms): "
              << compare_time.count() << std::endl;
    std::cout << "Number of self-loop checks: " << loop_checks << std::endl;
    std::cout << "Time spent loop checking (ms): "
              << loop_check_time.count() << std::endl;
}
