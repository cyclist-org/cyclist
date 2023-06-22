#include "heighted_graph.hpp"
#include "sloped_relation.hpp"
#include "types.c"

#include <cassert>
#include <chrono>
#include <cmath>
#include <iostream>
#include <fstream>
#include <set>
#include <unordered_set>
#include <thread>
#include <utility>

//=============================================================
// Flag init
//=============================================================
// N.B. These MUST match the corresponding constants in the OCaml code
//      Look in soundcheck.ml
const int Heighted_graph::FAIL_FAST        = 0b00000001;
const int Heighted_graph::USE_SCC_CHECK    = 0b00000010;
const int Heighted_graph::USE_IDEMPOTENCE  = 0b00000100;
const int Heighted_graph::USE_MINIMALITY   = 0b00001000;
const int Heighted_graph::COMPUTE_FULL_CCL = 0b00010000;
const int Heighted_graph::USE_SD           = 0b00100000;
const int Heighted_graph::USE_XSD          = 0b01000000;
const int Heighted_graph::USE_SLA          = 0b10000000;

//=============================================================
// Constructors / Destructor
//=============================================================
Heighted_graph::Heighted_graph(int max_nodes) {

    assert(max_nodes >= 0);

    this->max_nodes = max_nodes;
    this->num_edges_ = 0;

#ifdef LOG_STATS
    ccl_initial_size = 0;
    ccl_size = 0;
    ccl_iterations = 0;
    ccl_replacements = 0;
    ccl_rejections = 0;
    compositions = 0;
    comparisons = 0;
    loop_checks = 0;
    checked_size_sum = 0;
    total_size_sum = 0;

    compose_time = std::chrono::duration<double, DURATION>::zero();
    compare_time = std::chrono::duration<double, DURATION>::zero();
    need_to_add_compute_time = std::chrono::duration<double, DURATION>::zero();
    insertion_time = std::chrono::duration<double, DURATION>::zero();
    loop_check_time = std::chrono::duration<double, DURATION>::zero();
#endif

    h_change =
        (Sloped_relation***) malloc(sizeof(Sloped_relation**) * max_nodes);

    for(int i = 0; i < max_nodes; i++) {
        h_change[i] =
            (Sloped_relation**) malloc(sizeof(Sloped_relation*) * max_nodes);
        for (int j = 0; j < max_nodes; j++) {
            h_change[i][j] = NULL;
        }
    }

}

Heighted_graph::~Heighted_graph(void) {
    for (Map<int, int>* idx_map : height_idxs) {
        if (idx_map) delete idx_map;
    }
    for (Int_SET* heights : HeightsOf) {
        if (heights) delete heights;
    }
    for (int source = 0; source < max_nodes; source++) {
        for (int sink = 0; sink < max_nodes ; sink++) {
            Sloped_relation* R = h_change[source][sink];
            if (R != NULL) {
                delete R;
            }
        }
        free(h_change[source]);
    }
    free(h_change);
}

//=============================================================
// Getters
//=============================================================
int Heighted_graph::num_nodes(void) {
    return node_idxs.size();
}

int Heighted_graph::num_edges(void) {
    return num_edges_;
}

slope Heighted_graph::get_slope(int src, int sink, int h1, int h2) {

    // Get the internal IDs of the src and sink notes
    int _src = node_idxs.at(src);
    int _sink = node_idxs.at(sink);

    if (h_change[_src][_sink] != 0) {

        // Get internal height ID maps for the nodes
        Map<int, int>* heights_src = height_idxs[_src];
        Map<int, int>* heights_sink = height_idxs[_sink];

        // If the heights exists in the nodes, look up and return the slope
        auto src_it = heights_src->find(h1);
        if (src_it != heights_src->end()) {
            auto sink_it = heights_sink->find(h2);
            if (sink_it != heights_sink->end()) {
                int _h1 = src_it->second;
                int _h2 = sink_it->second;
                return h_change[src][sink]->get_slope(_h1, _h2);
            }
        }

    }

    // If no edge, or heights do not exist in the nodes
    return Undef;
}

//=============================================================
// Setters
//=============================================================

int Heighted_graph::parse_flags(const std::string flags_s) {
    int flags = 0;
    for (char c : flags_s) {
        switch (c) {
            case 'f': flags |= FAIL_FAST; break;
            case 's': flags |= USE_SCC_CHECK; break;
            case 'i': flags |= USE_IDEMPOTENCE; break;
            case 'm': flags |= USE_MINIMALITY; break;
            case 'a': flags |= COMPUTE_FULL_CCL; break;
            case 'D': flags |= USE_SD; break;
            case 'X': flags |= USE_XSD; break;
            case 'A': flags |= USE_SLA; break;
        }
    }
    assert(((flags & USE_SD) == 0) || ((flags & USE_XSD) == 0));
    return flags;
}

//=============================================================
// Output / Debug
//=============================================================

void print_ccl(Relation_LIST*** ccl, int num_nodes) {
    for (int source = 0; source < num_nodes; source++) {
    for (int sink = 0; sink < num_nodes; sink++) {
        for (auto R : *ccl[source][sink]) {
            std::cout << "<><><><><><><><><><><><><><><><><><><><><><><><><><><><><>" << std::endl;
            std::cout << source << " " << sink << std::endl;
            std::cout << R;
            std::cout << "<><><><><><><><><><><><><><><><><><><><><><><><><><><><><>" << std::endl;
        }
    }
    }
}

void Heighted_graph::print_statistics(void) {
    print_flags(this->flags);
#ifdef LOG_STATS
    std::cout << "Initial CCL size: " << ccl_initial_size << std::endl;
    std::cout << "Final CCL size: " << ccl_size << std::endl;
    std::cout << "Number of iterations to compute CCL: " << ccl_iterations << std::endl;
    std::cout << "CCL Rejections: " << ccl_rejections << std::endl;
    std::cout << "CCL Replacements: " << ccl_replacements << std::endl;
    std::cout << "Sloped relations computed: " << compositions << std::endl;
    std::cout << "Time spent computing sloped relations (ms): "
              << compose_time.count() << std::endl;
    std::cout << "Sloped relations compared: " << comparisons << std::endl;
    std::cout << "Time spent computing \"need to add\" (ms): "
              << need_to_add_compute_time.count() << std::endl;
    std::cout << "(of which) time spent comparing sloped relations (ms): "
              << compare_time.count() << std::endl;
    std::cout << "Time spent inserting sloped relations (ms): "
              << insertion_time.count() << std::endl;
    std::cout << "Number of self-loop checks: " << loop_checks << std::endl;
    std::cout << "Time spent loop checking (ms): "
              << loop_check_time.count() << std::endl;
    std::cout << "Average size of loop-checked sloped relations: "
              << ((loop_checks == 0) ? 0 : (checked_size_sum / loop_checks))
              << std::endl;
    std::cout << "Average size of all computed sloped relations: "
              << ((compositions == 0) ? 0 : (total_size_sum / compositions))
              << std::endl;
#endif
}

void Heighted_graph::print_flags(int flags) {
    if ((flags & FAIL_FAST) != 0) std::cout << "FAIL_FAST" << std::endl;
    if ((flags & USE_SCC_CHECK) != 0) std::cout << "USE_SCC_CHECK" << std::endl;
    if ((flags & USE_IDEMPOTENCE) != 0) std::cout << "USE_IDEMPOTENCE" << std::endl;
    if ((flags & USE_MINIMALITY) != 0) std::cout << "USE_MINIMALITY" << std::endl;
    if ((flags & COMPUTE_FULL_CCL) != 0) std::cout << "COMPUTE_FULL_CCL" << std::endl;
    if ((flags & USE_SD) != 0) std::cout << "USE_SD" << std::endl;
    if ((flags & USE_XSD) != 0) std::cout << "USE_XSD" << std::endl;
}

//=============================================================
// Methods for constructing the height graph
//=============================================================
void Heighted_graph::add_node(int n) {
    if (node_idxs.find(n) == node_idxs.end()) {
        int next_idx = node_idxs.size();
        assert(next_idx < max_nodes);
        node_idxs[n] = next_idx;
        height_idxs.push_back(new Map<int, int>());
        HeightsOf.push_back(new Int_SET());
    }
}

void Heighted_graph::add_height(int n, int h) {

    add_node(n);

    // for each node the external height IDs are mapped in an increasing order,
    // i.e the internal heights IDs are local -- node 1 and 2 might both contain
    // height with external ID 4, the respective corresponding interal IDs might
    // be different

    int n_idx = node_idxs.at(n);
    Map<int, int>* heights = height_idxs[n_idx];
    if (heights->find(h) == heights->end()) {
        int next_idx = heights->size();
        heights->insert(Int_pair(h, next_idx));
    }
    
    int h_idx = heights->at(h);
    HeightsOf[n_idx]->insert(h_idx);
    if( HeightsOf[n_idx]->size() > this->trace_width ){
        this->trace_width =  HeightsOf[n_idx]->size();
    }

}

void Heighted_graph::add_edge(int source, int sink) {
    add_node(source);
    add_node(sink);
    int src_idx = node_idxs.at(source);
    int sink_idx = node_idxs.at(sink);
    if (h_change[src_idx][sink_idx] == NULL) {
        num_edges_++;
        int num_src_heights = height_idxs[src_idx]->size();
        int num_dst_heights = height_idxs[sink_idx]->size();
        Sloped_relation* R = 
            new Sloped_relation(num_src_heights, num_dst_heights);
        h_change[src_idx][sink_idx] = R;
    }
}

void Heighted_graph::add_hchange(int source, int source_h, int sink, int sink_h, slope s) {
    add_edge(source, sink);
    add_height(source, source_h);
    add_height(sink, sink_h);
    int src_idx = node_idxs[source];
    int src_h_idx = height_idxs[src_idx]->at(source_h);
    int sink_idx = node_idxs[sink];
    int sink_h_idx = height_idxs[sink_idx]->at(sink_h);
    h_change[src_idx][sink_idx]->add(src_h_idx, sink_h_idx, s);
}

void Heighted_graph::add_stay(int source_node, int source_h, int sink_node, int sink_h) {
    add_hchange(source_node, source_h, sink_node, sink_h, Stay);
}

void Heighted_graph::add_decrease(int source_node, int source_h, int sink_node, int sink_h) {
    add_hchange(source_node, source_h, sink_node, sink_h, Downward);
}

//=============================================================
// Relational Infinite Descent Check
//=============================================================

void relational_check_cleanup
    (
        Relation_LIST*** ccl,
        int num_nodes,
        Set<std::reference_wrapper<Sloped_relation>,
            Sloped_relation::linear_order>&
                representatives,
        Sloped_relation*** h_change,
        Sloped_relation* composition
    )
{
    
    // Free CCL
    for (int source = 0; source < num_nodes; source++) {
        for (int sink = 0; sink < num_nodes; sink++) {
            delete ccl[source][sink];
        }
        free(ccl[source]);
    }
    free(ccl);

    // First remove all the Sloped_relation objects stored in h_change from
    // the representatives set.
    // The correctness of this relies on the dimensions of ccl containing all
    // the relations that might occur in h_change; i.e. the dimensions of
    // h_change might be larger, but any elements outside the dimensions of ccl
    // are null.
    for (int source = 0; source < num_nodes; source++) {
    for (int sink = 0; sink < num_nodes; sink++) {
        Sloped_relation* R = h_change[source][sink];
        if (R != NULL) {
            representatives.erase(*R);
        }
    }
    }
    // Then delete remaining Sloped_relation objects
    for (Sloped_relation& R : representatives) {
        delete &R;
    }

    // Delete temp Sloped_relation object for storing composition result
    if (composition != NULL) {
        delete composition;
    }

}

bool Heighted_graph::relational_check(int opts){

    this->flags = opts;

    // if ((opts & FAIL_FAST) != 0) std::cout << "Fail Fast\n";
    // if ((opts & USE_SCC_CHECK) != 0) std::cout << "Use SCC Check\n";
    // if ((opts & USE_IDEMPOTENCE) != 0) std::cout << "Use Idempotence\n";
    // if ((opts & USE_MINIMALITY) != 0) std::cout << "Use Minimality\n";

    // We cannot combine the idempotence and minimality optimisations.
    assert(((opts & USE_IDEMPOTENCE) == 0) || ((opts & USE_MINIMALITY) == 0));
    // It doesn't make sense to combine the idempotence and the SCC-based loop check
    assert(((opts & USE_IDEMPOTENCE) == 0) || ((opts & USE_SCC_CHECK) == 0));

    /* N.B. Initially, we though that it is useless to combine the fast-fail and
            minimality optimisations, since the point of the minimality
            optimisation is to not have to check for self-loops in all sloped
            relations. So when applying the minimality optimisation, it can be
            that we replace a sloped relation with a self-loop by one without a
            self-loop. Therefore, if checking for self-loops on-the-fly, we
            would still end up having to check every sloped relation anyway.

            However, experimentally, we did not see sloped relations being
            replaced in the CCL when using the minimality optimisation. Instead,
            we only observed that newly computed relations were rejected,
            leading to the CCL being significantly smaller in some cases.
            So, this being the case, it **does** make sense to combine it with
            fast-fail: if we detect that we need to add a newly computed sloped
            relation to the CCL, at that point we check the self-loop, and
            otherwise we simply reject the relation and fail.
     */

    // Number of actual nodes in the graph
    int num_nodes = this->num_nodes();

    // Maintain a set of unique representative objects for sloped relation
    Set<std::reference_wrapper<Sloped_relation>, Sloped_relation::linear_order>
        representatives;

    // A single sloped relation for storing the result of composing relations
    Sloped_relation* composition = NULL;

    // Allocate array to hold CCL, and initialise it with the
    // (unique representatives of) relations stored in the h_change field
    // If FAIL_FAST flag is set, also check appropriate relations for self-loops
    Relation_LIST*** ccl =
        (Relation_LIST***) malloc(sizeof(Relation_LIST**) * num_nodes);
    for(int i = 0; i < num_nodes; i++) {
        ccl[i] = (Relation_LIST**) malloc(sizeof(Relation_LIST*) * num_nodes);
        for (int j = 0; j < num_nodes; j++) {
            ccl[i][j] = new Relation_LIST();
            // If there is sloped relation for the corresponding edge
            if (h_change[i][j] != NULL) {
                // Get unique representative of the sloped relation
                Sloped_relation& rep =
                    *(representatives.insert(*h_change[i][j]).first);
                // Ensure the relation is initialised
                rep.initialize();
                // Add it to the CCL
                ccl[i][j]->push_back(&rep);
#ifdef LOG_STATS
                ccl_initial_size++;
                ccl_size++;
#endif
            }
        }
    }

    // If fail-fast, then need to check initial sloped relations for self-loops
    // We could have done this inline with the initialisation of the CCL, but
    // given that we will need to reclaim the memory allocated for the CCL in
    // the case that the check fails, it is more convenient to do this if the
    // CCL memory allocation has fully completed. So we do the initial self-loop
    // checks separately here
    if ((opts & FAIL_FAST) != 0) {
        if (!check_Ccl(ccl, opts)) {
            relational_check_cleanup(ccl, num_nodes, representatives, h_change, composition);
            return false;
        }
    }

#ifdef LOG_STATS
    std::chrono::time_point<std::chrono::system_clock> start;
    std::chrono::time_point<std::chrono::system_clock> end;
#endif

    // A set for keeping track of sloped relations that have been considered
    // for the closure element currently being computed
    Set<Sloped_relation*> visited;

    // A list of sloped relations that need to be removed from the current
    // closure element once it has been computed
    Relation_LIST preceded;

    // initialise sloped relation for storing the result of composing relations
    composition = new Sloped_relation(this->trace_width, this->trace_width);

    // Now compute the order-reduced closure
    for (int source = 0; source < num_nodes; source++) {
    for (int sink = 0; sink < num_nodes; sink++) {

        // Initialise contents of visited set for this closure element
        for (Sloped_relation* R : *ccl[source][sink]) {
            visited.insert(R);
        }

        for (int middle = 0; middle < source && middle < sink; middle++) {
            for (
                auto left = ccl[source][middle]->begin();
                left != ccl[source][middle]->end();
                left++
            ) {
                Sloped_relation* P = *left;
                for (
                    auto right = ccl[middle][sink]->begin();
                    right != ccl[middle][sink]->end();
                    right++
                ) {
                    Sloped_relation* Q = *right;
#ifdef LOG_STATS
                    start = std::chrono::system_clock::now();
#endif
                    // Compute the composition of P and Q in-place
                    composition->reset();
                    composition->compose(*P, *Q);
                    // Get the unique representative of the composition
                    auto insert_result = representatives.insert(*composition);
                    Sloped_relation& R = *(insert_result.first);
                    // If the representative is the object just computed
                    // we need to initialise it, and then allocate a new object
                    // for holding the results of composition
                    if (insert_result.second) {
                        R.initialize();
                        composition =
                            new Sloped_relation (
                                    this->trace_width,
                                    this->trace_width
                                );
                    }

#ifdef LOG_STATS
                    end = std::chrono::system_clock::now();
                    compose_time += (end - start);
                    compositions++;
                    total_size_sum += R->size();
#endif

                    bool fresh =
                        check_and_add(
                            *ccl[source][sink], R, visited, preceded,
                            // N.B. Passing the end() iterator as the endpoint
                            // of the preserved portion indicates that no
                            // relations should be preserved.
                            ccl[source][sink]->end(),
                            opts
                        );

                    // If fail-fast, then check for self-loop if necessary
                    if (fresh && ((opts & FAIL_FAST) != 0) && source == sink
                                && !(check_self_loop(R, opts))) {
                        relational_check_cleanup(ccl, num_nodes, representatives, h_change, composition);
                        return false;
                    }
                }
            }
        }

        // If the closure entry is empty, we can skip to the next iteration
        if (ccl[source][sink]->size() == 0) {
            continue;
        }

        // Now "tie the loops"
        // We need to remember the initial end point of ccl[source][sink]
        // because the following loops will add to it
        // N.B. Since the closure entry is not empty at this point, we take the
        // iterator pointing to the last element included in the portion to be
        // preserved. Since the code in the body of the loops will not remove
        // any relations up to and including this element, this iterator will
        // not be invalidated.
        auto initial_end = --(ccl[source][sink]->end());
        if (source > sink) {
            bool done = false;
            for (auto left = ccl[source][sink]->begin(); !done; left++) {
                Sloped_relation* P = *left;
                for (auto right = ccl[sink][sink]->begin();
                     right != ccl[sink][sink]->end();
                     right++
                ) {
                    Sloped_relation* Q = *right;
#ifdef LOG_STATS
                    start = std::chrono::system_clock::now();
#endif
                    // Compute the composition of P and Q in-place
                    composition->reset();
                    composition->compose(*P, *Q);
                    // Get the unique representative of the composition
                    auto insert_result = representatives.insert(*composition);
                    Sloped_relation& R = *(insert_result.first);
                    // If the representative is the object just computed
                    // we need to initialise it, and then allocate a new object
                    // for holding the results of composition
                    if (insert_result.second) {
                        R.initialize();
                        composition =
                            new Sloped_relation (
                                    this->trace_width,
                                    this->trace_width
                                );
                    }
#ifdef LOG_STATS
                    end = std::chrono::system_clock::now();
                    compose_time += (end - start);
                    compositions++;
                    total_size_sum += R->size();
#endif

                    check_and_add(
                        *ccl[source][sink], R, visited, preceded,
                        initial_end, opts
                    );

                }
                // If we processed the last element of the initial segment,
                // set the flag to exit the loop
                if (left == initial_end) {
                    done = true;
                }
            }
        } else if (source == sink) {
            auto right = ccl[source][sink]->begin();
            while (right != ccl[source][sink]->end()) {
                Sloped_relation* Q = *right;
                bool done = false;
                for (auto left = ccl[source][sink]->begin(); !done; left++) {
                    Sloped_relation* P = *left;
#ifdef LOG_STATS
                    start = std::chrono::system_clock::now();
#endif
                    // Compute the composition of P and Q in-place
                    composition->reset();
                    composition->compose(*P, *Q);
                    // Get the unique representative of the composition
                    auto insert_result = representatives.insert(*composition);
                    Sloped_relation& R = *(insert_result.first);
                    // If the representative is the object just computed
                    // we need to initialise it, and then allocate a new object
                    // for holding the results of composition
                    if (insert_result.second) {
                        R.initialize();
                        composition =
                            new Sloped_relation (
                                    this->trace_width,
                                    this->trace_width
                                );
                    }
#ifdef LOG_STATS
                    end = std::chrono::system_clock::now();
                    compose_time += (end - start);
                    compositions++;
                    total_size_sum += R->size();
#endif

                    bool fresh =
                        check_and_add(
                            *ccl[source][sink], R, visited, preceded,
                            initial_end, opts
                        );

                    // If fail-fast, then check for self-loop if necessary
                    if (fresh && ((opts & FAIL_FAST) != 0) && source == sink
                                && !(check_self_loop(R, opts))) {
                        relational_check_cleanup(ccl, num_nodes, representatives, h_change, composition);
                        return false;
                    }
                    // If we processed the last element of the initial segment,
                    // set the flag to exit the loop
                    if (left == initial_end) {
                        done = true;
                    }
                }
                // Now move to the next relation in the queue
                right++;
            }
        }

        // Now remove any relations from the preserved segment that have been
        // recorded as being preceded by a subsequently added relation.
        // We can do this by walking along the preceded list in lock-step,
        // because the relations appear in the preceded list in the relative
        // order that they appear in the closure element.
        if ((opts & USE_MINIMALITY) != 0) {
            auto to_remove = preceded.begin();
            for (auto it = ccl[source][sink]->begin();
                 it != ccl[source][sink]->end() && to_remove != preceded.end();
                 it++)
            {
                // We can compare pointers here, because the elements of the
                // [preceded] vector are exactly the pointers used in the
                // closure element vector.
                if (*to_remove == *it) {
                    // Remove the relation from the closure element
                    it = ccl[source][sink]->erase(it);
                    // Decrement iterator as it will automatically increment
                    // in the next loop iteration
                    it--;
                    // Make current the next element to remove
                    to_remove++;
                }
            }
            // Now clear the list of preceded relations
            preceded.clear();
        }

        // Clear contents of the visited set for the next iteration
        visited.clear();
    }
    }

    // If not using fast-fail, then check for self-loops in the computed CCL
    if ((opts & FAIL_FAST) == 0) {
        if (!check_Ccl(ccl, opts)) {
            relational_check_cleanup(ccl, num_nodes, representatives, h_change, composition);
            return false;
        }
    }

    relational_check_cleanup(ccl, num_nodes, representatives, h_change, composition);

    return true;
}

bool Heighted_graph::check_and_add
     (
        Relation_LIST& entry,
        Sloped_relation& R,
        Set<Sloped_relation*>& visited,
        Relation_LIST& preceded,
        Relation_LIST::iterator preserve_end,
        int opts
     )
{

    // Add relation R to the visited set if it was not there already
#ifdef LOG_STATS
    start = std::chrono::system_clock::now();
#endif
    auto visited_res = visited.insert(&R);
#ifdef LOG_STATS
    auto loop_end = std::chrono::system_clock::now();
    need_to_add_compute_time += (loop_end - loop_start);
#endif
    if (!(visited_res.second)) {
        // If we've seen the relation before, nothing more to do
#ifdef LOG_STATS
        ccl_rejections++;
#endif
        return false;
    }
#ifdef LOG_STATS
    // If we get here, the relation was fresh and was already inserted
    insertion_time += (end - start);
#endif
    
    // If we are only storing the minimal elements then check if R is preceded
    // by any relation in the current state of the closure element and remove
    // (or, for those relations that need preserving, mark as removed) any
    // relations preceded by R
    bool add = true;
    if ((opts & USE_MINIMALITY) != 0) {

        // Initialise flag indicating whether we are in the preserved portion
        bool preserve = true;
        // If the preserve end iterator points to the end of the list, this
        // means no relations are to be preserved.
        if (preserve_end == entry.end()) {
            preserve = false;
        }

        // Iterator into the current list of relations marked as being preceded
        auto cur_preceded = preceded.begin();

        for (auto it = entry.begin(); it != entry.end(); it++) {

            // Get the next relation
            Sloped_relation* S = *it;

            // If the current relation is already in the list of (preserved but)
            // preceded relations, then advance iterator to next one,
            // but no need to compare it with the current candidate relation R
            if (*cur_preceded == S) {

                cur_preceded++;
#ifdef LOG_STATS
                ccl_replacements++;
#endif

            }
            // Otherwise, we do need to compare the current candidate R with the
            // current relation S
            else {
            
                // Compare it with R
#ifdef LOG_STATS
                start = std::chrono::system_clock::now();
#endif
                comparison cmp_result = S->compare(R);
#ifdef LOG_STATS
                end = std::chrono::system_clock::now();
                compare_time += (end - start);
#endif
                
                // If < R
                if (cmp_result == lt) {
                    // N.B. When we maintain the invariant that the closure
                    // element is "thin" it cannot happen that there exist
                    // relations P and Q in the closure element such that
                    // P < R < Q. So, the relationswe will encounter in this
                    // loop that are related to R are either all above R or all
                    // below R. So, we reach this case the first time we
                    // encounter a relation below R. This means that we can
                    // break out of the loop now and we won't be missing any
                    // relations above R that need removing
                    add = false;
#ifdef LOG_STATS
                    ccl_rejections++;
#endif
                    break;
                }
                
                // If > R
                else if (cmp_result == gt) {
                    // If S in the preserved portion, don't remove it
                    // immediately but add it to the list of relations to be
                    // removed at the end
                    if (preserve) {
                        preceded.insert(cur_preceded, S);
                    }
                    // Otherwise, remove S from the closure element immediately
                    else {
                        // The erade method returns an iterator to the element
                        // immediately following the erased one
                        it = entry.erase(it);
                        // So decrement this result since the for loop with then
                        // increment it again
                        it--;
#ifdef LOG_STATS
                        ccl_replacements++;
#endif
                    }
                }

            }

            // If the preserve portion was non-empty and we have just processed
            // the last relation that it contains, then unset flag
            if (preserve && it == preserve_end) {
                preserve = false;
            }

        }
    }

    // If we have not found any relations preceding R, then add it to the
    // closure entry
    if (add) {
#ifdef LOG_STATS
    start = std::chrono::system_clock::now();
#endif
        entry.push_back(&R);
#ifdef LOG_STATS
    end = std::chrono::system_clock::now();
    insertion_time += (end - start);
    ccl_size++;
#endif
    }

    return true;

}

bool Heighted_graph::check_self_loop(Sloped_relation& R, int opts) {

#ifdef LOG_STATS
    auto start = std::chrono::system_clock::now();
    loop_checks++;
    checked_size_sum += R->size();
#endif

    bool result = false;

    if ((opts & USE_SCC_CHECK) != 0) {
        result = R.has_downward_SCC();
    } else {
        // Compute R composed with R if using the idempotent method
        // or the transitive closure otherwise (i.e. using the standard method)
        Sloped_relation* R2 = 
            ((opts & USE_IDEMPOTENCE) != 0)
                ? R.compose(R)
                : R.compute_transitive_closure();

        // If we're using the idempotent method and the relation is not
        // idempotent then trivially return true
        if (((opts & USE_IDEMPOTENCE) != 0) && !(*R2 == R)) {
            result = true;
#ifdef LOG_STATS
            loop_checks--;
            checked_size_sum -= R->size();
#endif
        } else {
            // Otherwise, check we have a self-loop in the relevant relation
            result = R2->has_self_loop();
        }
        delete R2;
    }

#ifdef LOG_STATS
    auto end = std::chrono::system_clock::now();
    loop_check_time += end - start;
#endif

    return result;
}

bool Heighted_graph::check_Ccl(Relation_LIST*** ccl, int opts) {
    int num_nodes = this->num_nodes();
    for (int node = 0; node < num_nodes; node++) {
        for (Sloped_relation* R : *ccl[node][node]) {
            if (!check_self_loop(*R, opts)) {
                return false;
            }
        }
    }
    return true;
}


//=============================================================
// Slope Language Automata Construction for Infinite Descent
//=============================================================
bool Heighted_graph::sla_automata_check() {

    // BDD dictionary for the automata
    spot::bdd_dict_ptr    dict;
    
    // Number of atomic propositions (bits) to use to represent letters accepted
    // by the automata
    int64_t               ap_size = 0;
    
    // Initial states of the respective automata
    int                   s_init_ip = this->max_nodes;
    int                   s_init_tr = this->trace_width;
    
    // References to the automata objects themselves
    spot::twa_graph_ptr   aut_ipath;
    spot::twa_graph_ptr   aut_trace;

    // A vector of the unique sloped relations occurring in the graph
    Vec<Sloped_relation*> relation_vec;
    // Mapping from edges to unique ID of the sloped relation along the edge
    Map<Int_pair,int>     relation_idx;

    // Vector of BDDs corresponding to singleton atomic propositions (bits)
    Vec<bdd>              propositions;

    // Vector of BDDs corresponding to the sloped relations
    // Ordering of this vector matches up with the [relation_vec] vector
    Vec<bdd>              idx_encoding;

    // Construct unique IDs for each distinct sloped relation in the graph
    for( int src = 0 ; src < max_nodes ; src++ ){
        for( int sink = 0 ; sink < max_nodes ; sink++ ){
            if( h_change[src][sink] == 0 ) continue;
            Sloped_relation* P = h_change[src][sink];
            P->initialize();
            int idx = 0;
            bool found = false;
            for (auto it = relation_vec.begin(); it != relation_vec.end(); it++) {
                Sloped_relation* Q = *it;
                if (P->compare(*Q) == eq) {
                    found = true;
                    break;
                } else {
                    idx++;
                }
            }
            if (!found) {
                relation_vec.push_back(P);
            }
            relation_idx.insert(Pair<Int_pair,int>(Int_pair(src,sink),idx));
        }
    }


    /*************************
     * Initialise the automata
     *************************/

    dict = spot::make_bdd_dict();
    aut_ipath = make_twa_graph(dict);
    aut_trace = make_twa_graph(dict);

    aut_ipath->set_buchi();
    aut_ipath->new_states(this->max_nodes + 1);
    aut_ipath->set_init_state(s_init_ip);

    aut_trace->set_buchi();
    aut_trace->new_states(this->trace_width + 1);
    aut_trace->set_init_state(s_init_tr);
    ap_size = ceil(log2(relation_vec.size()));


    /******************************
     * Register atomic propositions
     ******************************/

    for(int64_t i=0; i < ap_size; ++i) {
        std::stringstream ss;
        ss << "p_" << i;
        propositions.push_back( bdd_ithvar(aut_ipath->register_ap(ss.str())) );
        aut_trace->register_ap(ss.str());
    }


    /*********************************
     * Generate the BDDs for each unique sloped relation (ID)
     *********************************/

    for (int idx = 0; idx < relation_vec.size(); idx++) {
        bdd curr_bdd = bddtrue;
        int code = idx;
        for (int64_t i = 0 ; i < ap_size ; i++) {
            bdd b = propositions[i];
            curr_bdd &= ((code % 2) ? bdd_not(b) : b );
            code >>= 1;
        }
        idx_encoding.push_back(curr_bdd);
    }


    /*********************************
     * Create the automata transitions
     *********************************/

    //
    // Path automaton
    //

    // Set up transitions corresponding to graph edges
    for( int src = 0 ; src < max_nodes ; src++ ){
        for( int sink = 0 ; sink < max_nodes ; sink++ ){
            if( h_change[src][sink] == 0 ) continue;
            bdd curr_bdd = idx_encoding[relation_idx.at(Int_pair(src,sink))];
            aut_ipath->new_edge(src, sink, curr_bdd, {0});
        }
    }

    // Set up transitions from the initial state to the states corresponding
    // to graph nodes
    for (int sink = 0; sink < max_nodes; sink++) {
        bdd curr_bdd = bddfalse;
        for (int src = 0; src < max_nodes; src++) {
            if (h_change[src][sink] == NULL) continue;
            curr_bdd |= idx_encoding[relation_idx.at(Int_pair(src,sink))];
        }
        aut_ipath->new_edge(s_init_ip, sink, curr_bdd, {0});
    }

    //
    // Trace automaton
    //

    // Any relation takes init state to itself, with non-accepting transition
    aut_trace->new_edge(s_init_tr, s_init_tr, bddtrue);

    // Any relation takes init state to a height, with non-accepting transition
    for (int h = 0; h < this->trace_width; h++) {
        aut_trace->new_edge(s_init_tr, h, bddtrue);
    }

    // There are two edges between each pair of heights h1 and h2:
    //   An accepting edge, recognising all sloped relations R with R(h1, h2, down)
    //   A non-accepting edge, recognising all sloped relations R with R(h1, h2, stay)
    for (int h1 = 0 ; h1 < this->trace_width; h1++) {
        for (int h2 = 0 ; h2 < this->trace_width; h2++) {

            // initalise the BDDs for the accepting and non-accepting edges
            bdd letter_acc = bddfalse;
            bdd letter_non_acc = bddfalse;

            // Iterate through sloped relations and incorporate them into the BDDs
            for (int i = 0; i < relation_vec.size(); i++) {
                Sloped_relation* R = relation_vec[i];
                slope s = R->get_slope(h1, h2);
                if (s == Downward) {
                    letter_acc |= idx_encoding[i];
                } else if (s == Stay) {
                    letter_non_acc |= idx_encoding[i];
                }
            }

            aut_trace->new_edge(h1, h2, letter_non_acc);
            aut_trace->new_edge(h1, h2, letter_acc, {0});
        }
    }

    /*******************************
     * Perform the containment check
     *******************************/
    return spot::contains(aut_trace, aut_ipath);

}