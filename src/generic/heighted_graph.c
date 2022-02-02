#include "heighted_graph.hpp"
#include "sloped_relation.hpp"
#include "types.c"
#include "graph.hpp"

#include <cassert>
#include <chrono>
#include <iostream>
#include <fstream>
#include <set>
#include <thread>
#include <utility>

// N.B. These MUST match the corresponding constants in the OCaml code
//      Look in soundcheck.ml
const int Heighted_graph::FAIL_FAST        = 0b000001;
const int Heighted_graph::USE_SCC_CHECK    = 0b000010;
const int Heighted_graph::USE_IDEMPOTENCE  = 0b000100;
const int Heighted_graph::USE_MINIMALITY   = 0b001000;
const int Heighted_graph::COMPUTE_FULL_CCL = 0b010000;
const int Heighted_graph::USE_SD           = 0b100000;

int Heighted_graph::parse_flags(const std::string flags_s) {
    int flags = 0;
    for (char c : flags_s) {
        switch (c) {
            case 'f': flags |= FAIL_FAST; break;
            case 's': flags |= USE_SCC_CHECK; break;
            case 'i': flags |= USE_IDEMPOTENCE; break;
            case 'm': flags |= USE_MINIMALITY; break;
            case 'A': flags |= COMPUTE_FULL_CCL; break;
            case 'D': flags |= USE_SD; break;
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

    rejected = new Relation_LIST();

    h_change_ =
        (Sloped_relation***) malloc(sizeof(Sloped_relation**) * max_nodes);
    Ccl = (Relation_LIST***) malloc(sizeof(Relation_LIST**) * max_nodes);

    for(int i = 0; i < max_nodes; i++) {
        Ccl[i] = (Relation_LIST**) malloc(sizeof(Relation_LIST*) * max_nodes);
        h_change_[i] =
            (Sloped_relation**) malloc(sizeof(Sloped_relation*) * max_nodes);
        for (int j = 0; j < max_nodes; j++) {
            h_change_[i][j] = 0;
            Ccl[i][j] = new Relation_LIST();
        }
    }
    edges = new std::vector<Int_pair>();

}

void clean_up(Relation_LIST*** ccl,
              Sloped_relation*** h_change_,
              int num_nodes,
              Relation_LIST* rejected) {

    for (int source = 0; source < num_nodes; source++) {
        for (int sink = 0; sink < num_nodes; sink++) {
            for (Sloped_relation* s : *(ccl[source][sink])) {
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
        if( heights ) delete heights;
    }
    if( edges ) delete edges;
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
        max_heights.push_back(int(0));
    }
}

void Heighted_graph::add_height(int n, int h_) {
    add_node(n);
    int h;
    if (height_idxs.find(h_) == height_idxs.end()) {
        int next_idx = height_idxs.size();
        height_idxs.insert(Int_pair(h_,next_idx));
    }
    h = height_idxs.at(h_);
    int idx = node_idxs.at(n);
    HeightsOf[idx]->insert(h);
    if( max_heights[idx] < h ) max_heights[idx] = h;
    if( max_height < h ) max_height = h;
}

void Heighted_graph::add_edge(int source, int sink) {
    add_node(source);
    add_node(sink);
    int src_idx = node_idxs[source];
    int sink_idx = node_idxs[sink];
    if ( h_change_[src_idx][sink_idx] == 0 ) {
        number_of_edges++;
        edges->push_back(Int_pair(src_idx,sink_idx));
        Sloped_relation* R = new Sloped_relation(max_heights[src_idx],max_heights[sink_idx]);
        h_change_[src_idx][sink_idx] = R;
        Ccl[src_idx][sink_idx]->push_back(R);
        ccl_initial_size++;
        ccl_size++;
    }
}

void Heighted_graph::add_hchange(int source, int source_h, int sink, int sink_h, slope s) {
    add_edge(source, sink);
    int src_idx = node_idxs[source];
    int sink_idx = node_idxs[sink];
    h_change_[src_idx][sink_idx]->add(height_idxs[source_h], height_idxs[sink_h], s);
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
        rejected->push_back(R2);
    }

    auto end = std::chrono::system_clock::now();
    loop_check_time += end - start;

    return result;
}

bool Heighted_graph::check_Ccl(int opts) {
    int num_nodes = this->num_nodes();
    for( int node = 0; node < num_nodes; node++ ){
        Relation_LIST* Ccl_nd = Ccl[node][node];
        for( Sloped_relation* R : *Ccl_nd ){
            R->initialize();
            if(!check_self_loop(R, node, opts)) {
                return false;
            }
        }
    }
    return true;
}

bool Heighted_graph::relational_check(int opts){

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

    // If fail-fast, then need to check initial sloped relations for self-loops
    if ((opts & FAIL_FAST) != 0) {
        if (!check_Ccl(opts)) {
            return false;
        }
    }

    std::chrono::time_point<std::chrono::system_clock> start;
    std::chrono::time_point<std::chrono::system_clock> end;

    int num_nodes = this->num_nodes();

    // Now compute the CCL
    bool done;
    do {
        ccl_iterations++;
        // reset loop flag
        done = true;
        for (int source = 0; source < num_nodes; source++) {
        for (int sink = 0; sink < num_nodes; sink++) {
        for (int middle = 0; middle < num_nodes; middle++) {


            for (
                auto left = Ccl[source][middle]->begin();
                left != Ccl[source][middle]->end();
                left++
            ) {
                Sloped_relation* P = *left;
                P->initialize();
                if (P->size() == 0) continue;
                for (
                    auto right = Ccl[middle][sink]->begin();
                    right != Ccl[middle][sink]->end();
                    right++
                ) {
                    Sloped_relation* Q = *right;
                    Q->initialize();
                    if (Q->size() == 0) continue;
                    start = std::chrono::system_clock::now();
                    Sloped_relation* R = P->compose(*Q);
                    end = std::chrono::system_clock::now();
                    compose_time += (end - start);
                    compositions++;
                    total_size_sum += R->size();
                    if (R->size() == 0) continue;

                    bool need_to_add = true;
                    auto loop_start = std::chrono::system_clock::now();
                    for (
                        auto outer = Ccl[source][sink]->begin();
                        outer != Ccl[source][sink]->end();
                        outer++
                    ) {
                        Sloped_relation* S = *outer;
                        S->initialize();
                        comparisons++;
                        if ((opts & USE_MINIMALITY) != 0) {
                            start = std::chrono::system_clock::now();
                            comparison result = R->compare(*S);
                            end = std::chrono::system_clock::now();
                            compare_time += (end - start);
                            if (result == lt) {
                                ccl_replacements++;
                                ccl_size--;
                                auto to_delete = outer--;
                                if (to_delete == left) left--;
                                if (to_delete == right) right--;
                                (Ccl[source][sink])->erase(to_delete);
                                rejected->push_back(S);
                                break;
                            }
                            else if (result == eq || result == gt) {
                                ccl_rejections++;
                                need_to_add = false;
                                break;
                            }
                        } else {
                            start = std::chrono::system_clock::now();
                            bool equal = (*S == *R);
                            end = std::chrono::system_clock::now();
                            compare_time += (end - start);
                            if (equal) {
                                ccl_rejections++;
                                need_to_add = false;
                                break;
                            }
                        }
                    }
                    auto loop_end = std::chrono::system_clock::now();
                    need_to_add_compute_time += (loop_end - loop_start);

                    // If fail-fast, then check for self-loop if necessary
                    bool fail_now = false;
                    if (need_to_add 
                            && ((opts & FAIL_FAST) != 0)
                            && source == sink
                            && !(check_self_loop(R, source, opts))) {
                        rejected->push_back(R);
                        return false;
                    }

                    if (need_to_add) {
                        // Otherwise, not done with the fixed point computation
                        done = false;
                        // So add the newly computed sloped relation
                        start = std::chrono::system_clock::now();
                        (Ccl[source][sink])->push_back(R);
                        end = std::chrono::system_clock::now();
                        insertion_time += (end - start);
                        ccl_size++;
                    } else {
                        rejected->push_back(R);
                    }
                }
            }

        }
        }
        }
    } while (((opts & COMPUTE_FULL_CCL) != 0) && !done);

    // If not using fast-fail, then check for self-loops in the computed CCL
    if ((opts & FAIL_FAST) == 0) {
        if (!check_Ccl(opts)) {
            return false;
        }
    }

    return true;
}

bool Heighted_graph::sd_check() {
    int size_ = this->num_nodes();
    for( int node = 0; node < size_; node++ ){
    for( int node_ = 0; node_ < size_; node_++ ){
        Relation_LIST* Ccl_nd = Ccl[node][node_];
        for( Sloped_relation* R : *Ccl_nd ){
            R->initialize();
        }
    }
    }
    Graph G(edges,&HeightsOf,h_change_, num_nodes(),max_height);
    return G.check_SD();
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
}
