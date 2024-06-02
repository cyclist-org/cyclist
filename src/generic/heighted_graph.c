#include "heighted_graph.hpp"
#include "sloped_relation.hpp"
#include "types.c"

#include <algorithm>
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
const int Heighted_graph::FAIL_FAST        = 0b0000000001;
const int Heighted_graph::USE_SCC_CHECK    = 0b0000000010;
const int Heighted_graph::USE_IDEMPOTENCE  = 0b0000000100;
const int Heighted_graph::USE_MINIMALITY   = 0b0000001000;

const int Heighted_graph::PRINT_CCL        = 0b0000010000;

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

Heighted_graph* Heighted_graph::clone(Heighted_graph* hg)
{
    Heighted_graph* cloned = new Heighted_graph(hg->max_nodes);

    cloned->node_idxs = hg->node_idxs;
    
    cloned->height_idxs = std::vector<std::map<int,int>*>();
    for (size_t i = 0; i < cloned->height_idxs.size(); i++)
    {
        cloned->height_idxs[i] = new std::map<int,int>(*(hg->height_idxs[i]));
    };  
    
    cloned->HeightsOf = std::vector<std::set<int>*>(hg->HeightsOf.size());
    for (size_t i = 0; i < cloned->HeightsOf.size(); i++)
    {
        cloned->HeightsOf[i]= new std::set<int>(*(hg->HeightsOf[i]));
    };
    

    cloned->num_edges_ = hg->num_edges_;
    cloned->trace_width = hg->trace_width;
    for (size_t i = 0; i < cloned->max_nodes; i++)
    {
        for (size_t j = 0; j < cloned->max_nodes; j++)
        {
            if(hg->h_change[i][j]){
                cloned->h_change[i][j] = new Sloped_relation(*(hg->h_change[i][j]));
            }
        }
    }
    
    return cloned;
}

// Should return true iff [order] corresponds to an enumeration value of the
// NODE_ORDER enum
bool Heighted_graph::is_valid_node_order(int order) {

    switch (order) {
        case 0:
        case 1:
        case 2:
            return true;
    }

    return false;
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

Map<int, Int_SET*>* Heighted_graph::get_flat_edges() {
    Map<int, Int_SET *>* flat_edges = new Map<int, Int_SET *>();
    for (size_t i = 0; i < this->num_nodes(); i++)
    {
        flat_edges->insert(std::pair(i, new Int_SET()));
    }

    int num_nodes = this->num_nodes();
    for (size_t i = 0; i < num_nodes; i++)
    {
        for (size_t j = 0; j < num_nodes; j++)
        {
            if ((h_change[i][j] != NULL) && (!(h_change[i][j]->has_downward_slope())))
            {
                flat_edges->at(i)->insert(j);
            }
        }
    }
    return flat_edges;
}

Map<Int_pair, Int_pair_SET*>* Heighted_graph::get_stay_extended_edges() {
    Map<Int_pair, Int_pair_SET*>* edges = new Map<Int_pair, Int_pair_SET*>();
    int num_nodes = this->num_nodes();
    for (size_t i = 0; i < num_nodes; i++)
    {
        for (size_t j = 0; j < num_nodes; j++)
        {
            Sloped_relation* rel = h_change[i][j];
            if ((rel != NULL))
            {
                for (int i_height = 0; i_height < rel->get_num_src_heights(); i_height++) {
                for (int j_height = 0; j_height < rel->get_num_dst_heights(); j_height++) {
                    Int_pair src = Int_pair(i, i_height);
                    Int_pair sink = Int_pair(j, j_height);
                    slope slp = rel->get_slope(i_height, j_height);
                    if (slp == slope::Stay) {
                        if (edges->find(src) == edges->end()) {
                            (*edges)[src] = new Int_pair_SET();
                        }
                        (*edges)[src]->insert(sink);
                    }
                }
                }
            }
        }
    }

    return edges;
}

Map<Int_pair, Int_pair_SET*>* Heighted_graph::get_extended_edges() {
    Map<Int_pair, Int_pair_SET*>* edges = new Map<Int_pair, Int_pair_SET*>();
    int num_nodes = this->num_nodes();
    for (size_t i = 0; i < num_nodes; i++)
    {
        for (size_t j = 0; j < num_nodes; j++)
        {
            Sloped_relation* rel = h_change[i][j];
            if ((rel != NULL))
            {
                for (const auto& [edge, slp] : *(rel->get_slope_map())) {
                    int i_height = edge.first;
                    int j_height = edge.second;
                    Int_pair src = Int_pair(i, i_height);
                    Int_pair sink = Int_pair(j, j_height);

                    if (edges->find(src) == edges->end()) {
                        (*edges)[src] = new Int_pair_SET();
                    }
                    (*edges)[src]->insert(sink);
                }
            }
        }
    }

    return edges;
}

Vec<Int_pair> * Heighted_graph::get_edges() {
    Vec<Int_pair>* edges = new Vec<Int_pair>();
    int num_nodes = this->num_nodes();
    for (size_t i = 0; i < num_nodes; i++)
    {
        for (size_t j = 0; j < num_nodes; j++)
        {
            if (this->h_change[i][j] != NULL)
            {
                edges->push_back(Int_pair(i,j));
            }
        }
    }
    return edges;
}

int Heighted_graph::get_max_nodes() {
    return this->max_nodes;
}
Sloped_relation*** Heighted_graph::get_h_change() {
    return this->h_change;
}
std::vector<Int_SET*>* Heighted_graph::get_HeightsOf() {
    return &this->HeightsOf;
}

//=============================================================
// SCCs
//=============================================================

bool is_extended_edge_in_any_SCC(int source_idx, int source_h_idx, int sink_idx, int sink_h_idx, Vec<Set<Int_pair>>& SCCs){
    Int_pair source(source_idx, source_h_idx);
    Int_pair sink(sink_idx, sink_h_idx);
    for (const auto& SCC : SCCs) {
        if ((SCC.find(source) != SCC.end()) && (SCC.find(sink) != SCC.end())) {
            return true;
        }
    }
    return false;
}

/**
 * Tarjan's algorithm for finding strongly-connected components.
 * Reference: https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
 */
void Heighted_graph::find_scc_and_remove_down_edges_not_in_it
    (
        int n,
        std::stack<int>& s,
        bool* on_stack,
        int* idxs,
        int* low_links,
        int& next_idx,
        Vec<Int_pair>& extended_nodes,
        Map<Int_pair, int>& extended_nodes_idxs
    )
{
    Int_pair extended_node = extended_nodes[n];
    int node_idx = extended_node.first;
    int height_idx = extended_node.second;

    idxs[n] = next_idx++;
    low_links[n] = idxs[n];
        
    on_stack[n] = true;
    s.push(n);

    // Consider successors of n
    for (int neighbour_node_idx = 0; neighbour_node_idx < this->num_nodes(); neighbour_node_idx++) {
        Sloped_relation* edge_relation = this->h_change[node_idx][neighbour_node_idx];
        if(edge_relation == NULL) {
            continue;
        }

        Vec<Int_pair> neighbour_heights_idxs_and_slopes = edge_relation->get_height_neighbours(height_idx);
        for (Int_pair neighbour_height_idx_and_slope : neighbour_heights_idxs_and_slopes) {
            int neighbour_height_idx = neighbour_height_idx_and_slope.first;
            int slp = neighbour_height_idx_and_slope.second;
            int m = extended_nodes_idxs.at(Int_pair(neighbour_node_idx, neighbour_height_idx));

            // If successor m has not been visited yet
            if (idxs[m] == -1) {
                // Recurse on it
                find_scc_and_remove_down_edges_not_in_it(m, s, on_stack, idxs, low_links, next_idx, extended_nodes, extended_nodes_idxs);
                low_links[n] = 
                    low_links[n] < low_links[m] ? low_links[n] : low_links[m];
                if (idxs[n]<low_links[m] && (slp == slope::Downward)) {
                    this->remove_hchange(node_idx, height_idx, neighbour_node_idx, neighbour_height_idx, static_cast<slope>(slp));
                }
            }
            // Otherwise, if successor is in the stack then it is in the current
            // strongly-connected component
            else if (on_stack[m]) {
                low_links[n] = 
                    low_links[n] < idxs[m] ? low_links[n] : idxs[m];
            }
            // Otherwise successor is in an already processed strongly-connected
            // component
            else {
                if (slp == slope::Downward) {
                    this->remove_hchange(node_idx, height_idx, neighbour_node_idx, neighbour_height_idx, slope::Downward);
                }
            }
        }
    }

    // If the extended node n is the root of a discovered strongly-connected
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

Vec<Int_pair> Heighted_graph::get_extended_nodes() {
    Set<Int_pair> extended_nodes;
    for (int node_idx = 0; node_idx < this->num_nodes(); node_idx++) {
    for (int neighbour_node_idx = 0; neighbour_node_idx < this->num_nodes(); neighbour_node_idx++) {
        Sloped_relation* edge_relation = this->h_change[node_idx][neighbour_node_idx];
        if(edge_relation == NULL){
            continue;
        }

        const int* const* relation_repr_matrix = edge_relation->get_repr_matrix();
        for (int node_height_idx=0; node_height_idx< edge_relation->get_num_src_heights(); node_height_idx++){
        for (int neighbour_height_idx=0; neighbour_height_idx< edge_relation->get_num_dst_heights(); neighbour_height_idx++){
            extended_nodes.insert(Int_pair(node_idx, node_height_idx));
            extended_nodes.insert(Int_pair(neighbour_node_idx, neighbour_height_idx));
        }
        }

    }
    }
    return Vec(extended_nodes.begin(), extended_nodes.end());
}

void Heighted_graph::remove_down_edges_not_in_any_SCC() {

    auto start = std::chrono::system_clock::now();
    Vec<Int_pair> extended_nodes = this->get_extended_nodes();
    auto end = std::chrono::system_clock::now();
    printf("get_extended_nodes took %ld us\n", (end-start).count());

    start = std::chrono::system_clock::now();

    int num_of_extended_nodes = extended_nodes.size();
    Map<Int_pair, int> extended_nodes_idxs;
    for (size_t i = 0; i < num_of_extended_nodes; i++)
    {
        extended_nodes_idxs[extended_nodes[i]] = i;
    }
    

    // Vec<Set<Int_pair>> SCCs;
    bool* on_stack = (bool*) malloc(num_of_extended_nodes * sizeof(bool));
    int*  idxs = (int*) malloc(num_of_extended_nodes * sizeof(int));
    int*  low_links = (int*) malloc(num_of_extended_nodes * sizeof(int));
    
    for (int i = 0 ; i < num_of_extended_nodes; i++) {
        on_stack[i] = false;
        idxs[i] = -1;
        low_links[i] = -1;
    }

    std::stack<int> s;

    bool found = false;
    int next_idx = 0;


    end = std::chrono::system_clock::now();
    printf("preparing to go over all SCCs took %ld us\n", (end-start).count());

    start = std::chrono::system_clock::now();

    for (int n = 0; n < num_of_extended_nodes; n++) {
        if (idxs[n] == -1) {
            find_scc_and_remove_down_edges_not_in_it(n, s, on_stack, idxs, low_links, next_idx, extended_nodes, extended_nodes_idxs);
        }
    }

    free(on_stack);
    free(idxs);
    free(low_links);

    end = std::chrono::system_clock::now();
    printf("going over all SCCs took %ld us\n", (end-start).count());
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
            case 'p': flags |= PRINT_CCL; break;
        }
    }
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
            std::cout << *R;
            std::cout << "<><><><><><><><><><><><><><><><><><><><><><><><><><><><><>" << std::endl;
        }
    }
    }
}

void print_ccl(Set<Sloped_relation*>*** ccl, int num_nodes) {
    for (int source = 0; source < num_nodes; source++) {
    for (int sink = 0; sink < num_nodes; sink++) {
        for (auto R : *ccl[source][sink]) {
            std::cout << "<><><><><><><><><><><><><><><><><><><><><><><><><><><><><>" << std::endl;
            std::cout << source << " " << sink << std::endl;
            std::cout << *R;
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
}

std::string Heighted_graph::to_string() {
    std::string result = "";
    for (size_t i = 0; i < this->max_nodes; i++) {
        for (size_t j = 0; j < this->max_nodes; j++) {
            Sloped_relation* curr_relation = this->h_change[i][j];
            if (curr_relation != NULL) {
                curr_relation->initialize();
                auto curr_slope_map = *curr_relation->get_slope_map();
                if (curr_slope_map.size() == 0) {
                    // There is an edge with no height changes
                    result += "[(" + std::to_string(i) + ",),(" + std::to_string(j) + ",),]";
                } else {
                    for (auto const& [heights, edge_slope] : curr_slope_map) {
                        result += "[(" + std::to_string(i) + "," + std::to_string(heights.first) + "),(" +
                        std::to_string(j) + "," + std::to_string(heights.second)  +")," + (edge_slope == slope::Stay ? "stay" : "down") +"]";
                    }
                }
            } 
        }
    }
    return result;
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

void Heighted_graph::remove_hchange(int source_idx, int source_h_idx, int sink_idx, int sink_h_idx, slope s) {
    Sloped_relation* rel = this->h_change[source_idx][sink_idx];
    if(rel != NULL) {
        rel->remove(source_h_idx, sink_h_idx, s);
    }
}

//=============================================================
// Order-reduced Relational Infinite Descent Check
//=============================================================

void common_cleanup
    (
        int num_nodes,
        Set<std::reference_wrapper<Sloped_relation>,
            Sloped_relation::linear_order>&
                representatives,
        Sloped_relation*** h_change,
        Sloped_relation* composition
    )
{
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

void order_reduced_cleanup
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

    common_cleanup(num_nodes, representatives, h_change, composition);
}

bool deg_out_in_asc(
        const Pair<int, Pair<int, int>>& lhs,
        const Pair<int, Pair<int, int>>& rhs
    )
{
    return
        lhs.second.first < rhs.second.first
            ||
        (lhs.second.first == rhs.second.first
            && lhs.second.second < rhs.second.second);
}

bool deg_out_in_desc(
        const Pair<int, Pair<int, int>>& lhs,
        const Pair<int, Pair<int, int>>& rhs
    )
{
    return
        lhs.second.first > rhs.second.first
            ||
        (lhs.second.first == rhs.second.first
            && lhs.second.second > rhs.second.second);
}

bool Heighted_graph::order_reduced_check(NODE_ORDER order, int opts) {
    bool should_halt = false;
    return this->order_reduced_check(order, opts, &should_halt);
}

bool Heighted_graph::order_reduced_check(NODE_ORDER order, int opts, bool* should_halt) {

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

    // Local copy of number of actual nodes in the graph, to avoid some derefences
    int num_nodes = this->num_nodes();

    // Maintain a set of unique representative objects for sloped relations
    Set<std::reference_wrapper<Sloped_relation>, Sloped_relation::linear_order>
        representatives;

    // A single sloped relation for storing the result of composing relations
    Sloped_relation* composition = NULL;

    // A set for keeping track of sloped relations that have been considered
    // for the closure element currently being computed
    Set<Sloped_relation*> visited;

    // A list of sloped relations that need to be removed from the current
    // closure element once it has been computed
    Relation_LIST preceded;

    // Allocate array to hold CCL
    Relation_LIST*** ccl =
        (Relation_LIST***) malloc(sizeof(Relation_LIST**) * num_nodes);
    for(int i = 0; i < num_nodes; i++) {
        ccl[i] = (Relation_LIST**) malloc(sizeof(Relation_LIST*) * num_nodes);
        for (int j = 0; j < num_nodes; j++) {
            ccl[i][j] = new Relation_LIST();
        }
    }

    // Use a pointer to avoid allocating memory unless necessary
    Vec<Pair<int, Pair<int, int>>>* node_order = NULL;

    // If we are not using the given order, compute the specified ordering and
    // store the result in [node_order]
    if (order != GIVEN_ORDER) {

        node_order = new Vec<Pair<int, Pair<int, int>>>(num_nodes);
        for (int i = 0; i < num_nodes; i++) {
            (*node_order)[i].first = i;
            if (order != GIVEN_ORDER) {
                // Compute the in/out-degree
                (*node_order)[i].second.first = 0;
                (*node_order)[i].second.second = 0;
                for (int j = 0; j < num_nodes; j++) {
                    // Update the out-degree
                    if (h_change[i][j] != NULL) {
                        (*node_order)[i].second.first++;
                    }
                    // Update the in-degree
                    if (h_change[j][i] != NULL) {
                        (*node_order)[i].second.second++;
                    }
                }
            }
        }
        // Sort the vector
        switch (order) {
            case DEGREE_OUT_IN_ASC:
                std::sort(node_order->begin(), node_order->end(), deg_out_in_asc);
                break;
            case DEGREE_OUT_IN_DESC:
                std::sort(node_order->begin(), node_order->end(), deg_out_in_desc);
                break;
        }

    }

    // initialise the CCL with the (unique representatives of) relations
    // stored in the h_change field, according to the node ordering computed
    // above when not using the given order
    for (int i = 0; i < num_nodes; i++) {
        int nd_i = (node_order != NULL) ? (*node_order)[i].first : i;
        for (int j = 0; j < num_nodes; j++) {
            int nd_j = (node_order != NULL) ? (*node_order)[j].first : j;
            // If there is a sloped relation for the corresponding edge
            Sloped_relation* R_ptr = h_change[nd_i][nd_j];
            if (R_ptr != NULL) {
                // Get unique representative of the sloped relation
                Sloped_relation& R = *(representatives.insert(*R_ptr).first);
                // Ensure the relation is initialised
                R.initialize();
                // Add it to the CCL
                ccl[i][j]->push_back(&R);
#ifdef LOG_STATS
                ccl_initial_size++;
                ccl_size++;
#endif
            }
        }
    }

    // Delete vector used for computing node ordering, if it was created
    if (node_order != NULL) {
        delete node_order;
    }

    // If fail-fast, then need to check initial sloped relations for self-loops
    // We could have done this inline with the initialisation of the CCL, but
    // given that we will need to reclaim the memory allocated for the CCL in
    // the case that the check fails, it is more convenient to do this if the
    // CCL memory allocation has fully completed. So we do the initial self-loop
    // checks separately here
    if ((opts & FAIL_FAST) != 0) {
        if (!check_Ccl(ccl, opts)) {
            if ((opts & PRINT_CCL) != 0) {
                print_ccl(ccl, num_nodes);
            }
            order_reduced_cleanup(ccl, num_nodes, representatives, h_change, composition);
            return false;
        }
    }

#ifdef LOG_STATS
    std::chrono::time_point<std::chrono::system_clock> start;
    std::chrono::time_point<std::chrono::system_clock> end;
#endif

    // Initialise sloped relation for storing the result of composing relations
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
                    if(*should_halt){
                        return false;
                    }
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
                                && !(check_self_loop(R, opts)))
                    {
                        if ((opts & PRINT_CCL) != 0) {
                            print_ccl(ccl, num_nodes);
                        }
                        order_reduced_cleanup(ccl, num_nodes, representatives, h_change, composition);
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
                // Q: can we not just use (left != initial_end) instead of !done?
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
                    if (fresh && ((opts & FAIL_FAST) != 0) 
                                /* && source == sink */ // Not necessary here, we know it's true
                                && !(check_self_loop(R, opts))) {
                        if ((opts & PRINT_CCL) != 0) {
                            print_ccl(ccl, num_nodes);
                        }
                        order_reduced_cleanup(ccl, num_nodes, representatives, h_change, composition);
                        return false;
                    }
                    // If we processed the last element of the initial segment,
                    // set the flag to exit the loop
                    if (left == initial_end) {
                        done = true;
                    }
                    // TODO: We should be able to make an optimisation here.
                    // If the current value of the iterator 'right' is one of
                    // the relations removed (or recorded as preceded by R)
                    // during the check_and_add above, then we can break out of
                    // this inner loop (i.e. set done = true also in this case).
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
            if ((opts & PRINT_CCL) != 0) {
                print_ccl(ccl, num_nodes);
            }
            order_reduced_cleanup(ccl, num_nodes, representatives, h_change, composition);
            return false;
        }
    }

    if ((opts & PRINT_CCL) != 0) {
        print_ccl(ccl, num_nodes);
    }

    order_reduced_cleanup(ccl, num_nodes, representatives, h_change, composition);

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

            // Get the next relation for comparison
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
                        // The erase method returns an iterator to the element
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

//===============================================================
// Flloyd-Warshall-Kleene Algorithm for checking Infinite Descent
// by computing the composition closure
//===============================================================
void fwk_cleanup
    (
        Set<Sloped_relation*>*** ccl,
        Set<Sloped_relation*>*** ccl_next,
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
            delete ccl_next[source][sink];
        }
        free(ccl[source]);
        free(ccl_next[source]);
    }
    free(ccl);
    free(ccl_next);

    common_cleanup(num_nodes, representatives, h_change, composition);
}

bool Heighted_graph::fwk_check(int opts) {
    bool should_halt = false;
    return this->fwk_check(opts, &should_halt);
}

bool Heighted_graph::fwk_check(int opts, bool* should_halt) {

    // Record flags
    this->flags = opts;

    // We cannot combine the idempotence and minimality optimisations.
    assert(((opts & USE_IDEMPOTENCE) == 0) || ((opts & USE_MINIMALITY) == 0));
    // It doesn't make sense to combine the idempotence and the SCC-based loop check
    assert(((opts & USE_IDEMPOTENCE) == 0) || ((opts & USE_SCC_CHECK) == 0));

    // Local copy of number of actual nodes in the graph, to avoid some derefences
    int num_nodes = this->num_nodes();

    // Maintain a set of unique representative objects for sloped relations
    Set<std::reference_wrapper<Sloped_relation>, Sloped_relation::linear_order>
        representatives;

    // A single sloped relation for storing the result of composing relations
    Sloped_relation* tmp_relation = NULL;

    // A set for keeping track of sloped relations that have been considered
    // for the closure element currently being computed
    Set<Sloped_relation*> visited;

    // A list of sloped relations that need to be removed from the current
    // closure element once it has been computed
    Relation_LIST preceded;

    // Allocate arrays to accumulate CCL and compute next iteration,
    // and initialise CCL accumulator with the (unique representatives of)
    // relations stored in the h_change field
    Set<Sloped_relation*>*** ccl =
        (Set<Sloped_relation*>***)
            malloc(sizeof(Set<Sloped_relation*>**) * num_nodes);
    Set<Sloped_relation*>*** ccl_next =
        (Set<Sloped_relation*>***)
            malloc(sizeof(Set<Sloped_relation*>**) * num_nodes);
    for(int i = 0; i < num_nodes; i++) {
        ccl[i] =
            (Set<Sloped_relation*>**)
                malloc(sizeof(Set<Sloped_relation*>*) * num_nodes);
        ccl_next[i] =
            (Set<Sloped_relation*>**)
                malloc(sizeof(Set<Sloped_relation*>*) * num_nodes);
        for (int j = 0; j < num_nodes; j++) {
            ccl[i][j] = new Set<Sloped_relation*>;
            ccl_next[i][j] = new Set<Sloped_relation*>;
            // If there is sloped relation for the corresponding edge
            if (h_change[i][j] != NULL) {
                // Get unique representative of the sloped relation
                Sloped_relation& rep =
                    *(representatives.insert(*h_change[i][j]).first);
                // Ensure the relation is initialised
                rep.initialize();
                // Add it to the CCL
                ccl[i][j]->insert(&rep);
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
            if ((opts & PRINT_CCL) != 0) {
                print_ccl(ccl, num_nodes);
            }
            fwk_cleanup(ccl, ccl_next, num_nodes, representatives, h_change, tmp_relation);
            return false;
        }
    }

    // Create identity sloped relation
    tmp_relation = 
        new Sloped_relation(this->trace_width, this->trace_width, Stay);
    // Get the unique representative of the relation
    auto insert_result = representatives.insert(*tmp_relation);
    Sloped_relation& identity = *(insert_result.first);
    // If the representative is the same object just computed then we need to
    // initialise it, and then allocate a new sloped relation object
    if (insert_result.second) {
        identity.initialize();
        tmp_relation = 
            new Sloped_relation(this->trace_width, this->trace_width);
    }

    // A list of sloped relations, for storing the result of the asteration step
    Relation_LIST asteration;

    // Now compute the composition closure, using Floyd-Warshall-Kleene
    for (int k = 0; k < num_nodes; k++) {
        
        // First, compute asteration/closure of ccl[k][k], which is used in
        // all iterations of the inner nested loop

        // Clear previous contents
        asteration.clear();

        // Start with an empty list of visited relations
        visited.clear();
        
        // If the identity sloped relation is not already in ccl[k][k], then
        // add it as the first element in the asteration list, and record
        // whether the identity relation appears first (needed later).
        bool identity_first = true;
        if (ccl[k][k]->find(&identity) == ccl[k][k]->end()) {
            // Add the identity relation to the asteration set
            asteration.push_front(&identity);
            // Record it as visited
            visited.insert(&identity);
        } else {
            identity_first = false;
        }
        
        // Initialise with contents of closure element
        for (Sloped_relation* R : *ccl[k][k]) {
            asteration.push_back(R);
        }
        
        // Now do the closure computation
        auto it1 = asteration.begin();
        while (it1 != asteration.end()) {

            // A flag indicating whether the iterator should be incremented at
            // the end of the loop body. In the case that the minimisation
            // optimisation is being use, the asteration set may be modified,
            // and so in some cases, the iterator gets updated within the loop
            // body and so shouldn't be incremented.
            bool increment_iterator = true;
            
            Sloped_relation* Q = *it1;
            
            for (Sloped_relation* P : *ccl[k][k]) {
            
                // Compute the composition of P and Q in-place
                tmp_relation->reset();
                tmp_relation->compose(*P, *Q);                
            
                // Get the unique representative of the composition
                auto insert_result = representatives.insert(*tmp_relation);
                Sloped_relation& R = *(insert_result.first);
            
                // If the representative is the same object just computed
                // we need to initialise it, and then allocate a new object
                // for holding the results of composition next time
                if (insert_result.second) {
                    R.initialize();
                    tmp_relation =
                        new Sloped_relation (
                                this->trace_width,
                                this->trace_width
                            );
                }

                // If the new relation already appears in ccl[k][k]
                // then nothing to do
                if (ccl[k][k]->find(&R) != ccl[k][k]->end()) {
                    continue;
                }
                
                // Otherwise check if we have processed this relation before,
                // adding it to the visited set if not
                auto visited_res = visited.insert(&R);

                // If this relation has not been previously processed, then we
                // may need to add it to the closure, as well as remove some
                // relations already present, depending on whether we are
                // applying the minimality optimisation
                if (visited_res.second) {

                    // Start out assuming by default that we should add it
                    bool add = true;

                    // If we are applying the minimality optimisation then we
                    // need to go through the asteration set: if we find any
                    // elements preceding R, we do not add R, and otherwise we
                    // remove any elements preceded by R
                    if ((opts & USE_MINIMALITY) != 0) {
                        // Go through the asteration set to remove any preceded
                        auto it2 = asteration.begin();
                        while (it2 != asteration.end())
                        {
                            // Get the next relation for comparison
                            Sloped_relation* S = *it2;
                            // Compare S with R
                            comparison cmp_result = S->compare(R);
                            // If S < R, we don't need to add R
                            if (cmp_result == lt) {
                                add = false;
                                break;
                            }
                            // If S > R, needs removing from the asteration set
                            else if (cmp_result == gt) {
                                // If S is also the element currently referenced
                                // by the outer iterator 'it1', we need to take
                                // this into account when removing it
                                if (it2 == it1) {
                                    if (it2 == asteration.begin()) {
                                        it2 = asteration.erase(it2);
                                        it1 = asteration.begin();
                                        increment_iterator = false;
                                    } else {
                                        it1--;
                                        it2 = asteration.erase(it2);
                                    }
                                }
                                // Otherwise, simply remove the relation
                                else {
                                    it2 = asteration.erase(it2);
                                }
                            }
                            // Otherwise, move to the next relation
                            else {
                                it2++;
                            }
                        }
                    }

                    // If we should still add it
                    if (add) {
                        // Then do so
                        asteration.push_back(&R);
                    }
                }
            }

            // Increment the iterator over the asteration set, if necessary
            if (increment_iterator) {
                it1++;
            }
        }

        // Now we compute the next approximation of the CCL, in ccl_next

        for (int i = 0; i < num_nodes; i++) {
        for (int j = 0; j < num_nodes; j++) {

            // If using the minimality optimisation, then clear the set for
            // recording processed relations
            if ((opts & USE_MINIMALITY) != 0) {
                visited.clear();
            }

            // Initialise ccl_next[i][j] with ccl[i][j] using copy assignment
            // Copy assignment needed since we need to preserve ccl[i][j], which
            // might be used in other iterations of the outer loops
            *ccl_next[i][j] = *ccl[i][j];

            // // TODO: The above method, using copy-assignment, appears to
            // // display a memory leak, where as manually clearing the old set
            // // and copying over the elements has a different memory profile.
            // // It would be good to figure out why.
            // ccl_next[i][j]->clear();
            // for (auto it = ccl[i][j]->begin(); it != ccl[i][j]->end(); it++)
            // {
            //     ccl_next[i][j]->insert(*it);
            // }

            // Now compute
            //     ccl[i][k] compose asteration(ccl[k][k]) compose ccl[k][j]
            // adding the results to ccl_next[i][j]
            // Note that i = k and j = k are special cases: then the asteration
            // is equal to some or all this composition already.
            // Note that to achieve uniformity in the code, the outermost and
            // innermost loops here serve as "dummy" loops in these special
            // cases, iterating only once by having the iteration condition
            // being that the iterator is equal to begin().
            bool left_done = false;
            for (auto left = ccl[i][k]->begin();
                 (i == k ? !left_done : left != ccl[i][k]->end());
                 // The following iteration expression only increments iterator
                 // if i != k
                 (i == k ? left : left++))
            {
                Sloped_relation* P = NULL;
                if (i != k) {
                    P = *left;
                } else {
                    // When i == k, we run this loop exactly once
                    left_done = true;
                }

                auto mid = asteration.begin();
                // If i == k or j == k, then we can skip the first element of
                // the asteration set, which is the unit sloped relation
                if (identity_first && (i == k || j == k)) {
                    mid++;
                }
                for (mid; mid != asteration.end(); mid++) {
                    Sloped_relation* Q = *mid;
                    Sloped_relation* PQ = NULL;
                    if (i == k) {
                        PQ = Q;
                    } else {
                        // Compute the composition of P and Q in-place
                        tmp_relation->reset();
                        tmp_relation->compose(*P, *Q);
                        // Get the unique representative of the composition
                        auto insert_result
                            = representatives.insert(*tmp_relation);
                        PQ = &(insert_result.first->get());
                        // If the representative is the object just computed
                        // we need to initialise it, and then allocate a new
                        // object for holding the results of composition
                        if (insert_result.second) {
                            PQ->initialize();
                        }
                        tmp_relation =
                            new Sloped_relation (
                                    this->trace_width,
                                    this->trace_width
                                );
                    }

                    bool right_done = false;
                    for (auto right = ccl[k][j]->begin();
                         k == j ? !right_done : right != ccl[k][j]->end();
                        // The following iteration expression only increments
                        // iterator if k != j
                        (k == j ? right : right++))
                    {
                        if(*should_halt){
                            return false;
                        }
                        Sloped_relation* PQR = NULL;
                        if (k == j) {
                            PQR = PQ;
                            // When k == j, we run this loop exactly once
                            right_done = true;
                        } else {
                            Sloped_relation* R = *right;
                            // Compute the composition of PQ and R in-place
                            tmp_relation->reset();
                            tmp_relation->compose(*PQ, *R);
                            // Get the unique representative of the composition
                            auto insert_result
                                = representatives.insert(*tmp_relation);
                            PQR = &(insert_result.first->get());
                            // If the representative is the object just computed
                            // we need to initialise it, and then allocate a new
                            // object for holding the results of composition
                            if (insert_result.second) {
                                PQR->initialize();
                                tmp_relation =
                                    new Sloped_relation (
                                            this->trace_width,
                                            this->trace_width
                                        );
                            }
                        }
                        bool added = true;
                        // If not using the minimality optimisation
                        if ((opts & USE_MINIMALITY) == 0) {
                            // simply try adding PQR to ccl_next[i][j] directly
                            auto res = ccl_next[i][j]->insert(PQR);
                            // If it was already there, then reset added flag
                            if (!res.second) {
                                added = false;
                            }
                        }
                        // Otherwise, we need to check if we have processed PQR
                        // previously and, if not, check whether it precedes or
                        // is preceded by other relations in ccl_next[i][j]
                        else {
                            // If PQR already appears in ccl[i][j], then we can
                            // simply continue computing the rest of the closure
                            // elements
                            if (ccl[i][j]->find(PQR) != ccl[i][j]->end()) {
                                continue;
                            }
                            // If not, check if we have processed PQR before
                            // adding it to the visited set if not
                            auto visited_res = visited.insert(PQR);
                            // If PQR has previously been processed (i.e. the
                            // insertion was not successful), then reset the
                            // added flag
                            if (!visited_res.second) {
                                added = false;
                            }
                            // Otherwise check if need to add it to the closure,
                            // or remove some relations already present
                            else {
                                // Start by assuming PQR needs to be added
                                bool add = true;
                                // Check relations currently in ccl_next[i][j]
                                auto it = ccl_next[i][j]->begin();
                                while (it != ccl_next[i][j]->end())
                                {
                                    // Get the next relation for comparison
                                    Sloped_relation* S = *it;
                                    // Compare S with PQR
                                    comparison cmp_result = S->compare(*PQR);
                                    // If S < PQR, we don't need to add PQR
                                    if (cmp_result == lt) {
                                        add = false;
                                        break;
                                    }
                                    // If S > PQR, S needs removing
                                    else if (cmp_result == gt) {
                                        // Remove S from the set
                                        it = ccl_next[i][j]->erase(it);
                                    } else {
                                        it++;
                                    }
                                }
                                // If we need to add PQR to ccl_next[i][j]
                                if (add) {
                                    // Then do so
                                    ccl_next[i][j]->insert(PQR);
                                }
                                // Otherwise reset flag recording whether added
                                else {
                                    added = false;
                                }
                            }
                        }

                        // If it was added and we are using fast-fail,
                        // then check for self-loop if necessary
                        if (added && ((opts & FAIL_FAST) != 0) && i == j
                                  && !(check_self_loop(*PQR, opts)))
                        {
                            if ((opts & PRINT_CCL) != 0) {
                                print_ccl(ccl, num_nodes);
                            }
                            fwk_cleanup(ccl, ccl_next, num_nodes, 
                                        representatives, h_change,
                                        tmp_relation);
                            return false;
                        }
                    }
                }
            }

        }
        }

        // Lastly, before next iteration, swap over references to
        // CCL accumulator and the array for storing the next approximation
        Set<Sloped_relation*>*** tmp = ccl;
        ccl = ccl_next;
        ccl_next = tmp;

    }

    // If not using fast-fail, check for self-loops in the fully computed CCL
    if ((opts & FAIL_FAST) == 0) {
        if (!check_Ccl(ccl, opts)) {
            if ((opts & PRINT_CCL) != 0) {
                print_ccl(ccl, num_nodes);
            }
            fwk_cleanup(ccl, ccl_next, num_nodes, representatives, h_change, tmp_relation);
            return false;
        }
    }

    if ((opts & PRINT_CCL) != 0) {
        print_ccl(ccl, num_nodes);
    }

    fwk_cleanup(ccl, ccl_next, num_nodes, representatives, h_change, tmp_relation);

    return true;
}

bool Heighted_graph::check_Ccl(Set<Sloped_relation*>*** ccl, int opts) {
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

//======================================================
// Automata Constructions for Checking Infinite Descent
//======================================================

/*
 * Creates and registers atomic propositions with the given automata,
 * using them as "bits" to create BDDs encoding binary representation of IDs
 * for each alphabet letter.
 * 
 * N.B. This assumes that [alphabet_vec] has already allocated space to store
 * at least [size] elements. Behaviour is undefined if this doesn't hold.
 * It also assumes that the elements stored in the vector are equal to bddtrue.
 */
void init_alphabet(
    int size,
    spot::twa_graph_ptr path_aut,
    spot::twa_graph_ptr trace_aut,
    Vec<bdd>& alphabet_vec
) {
    // First, register atomic propositions.
    // Note that atomic propositions are given an ID according to the order
    // that they are created in, so we could simply use loop counters below
    // when generating the associated BDDs using the bdd_ithvar function.
    // However, the documentation doesn't specify that the IDs are created like
    // this, so it's safer to store the IDs returned by the register_ap method
    // in case this changes in future releases.
    int ap_size = (size <= 0) ? 0 : ceil(log2(size));
    Vec<int> propositions(ap_size);
    for(int i=0; i < ap_size; ++i) {
        std::stringstream ss;
        ss << "p_" << i;
        propositions[i] = path_aut->register_ap(ss.str());
        int ap_idx = trace_aut->register_ap(ss.str());
        assert (propositions[i] == ap_idx);
    }

    // Now, generate the BDDs for each letter of the alphabet
    for (int idx = 0; idx < size; idx++) {
        assert (alphabet_vec[idx] == bddtrue);
        int code = idx;
        for (int i = 0; i < ap_size; i++) {
            bdd p = bdd_ithvar(propositions[i]);
            alphabet_vec[idx] &= ((code & 0b1) ? bdd_not(p) : p);
            code >>= 1;
        }
    }    
}

bool Heighted_graph::vla_automata_check() {

    // BDD dictionary for the automata
    spot::bdd_dict_ptr    dict;

    int                   num_nodes = this->num_nodes();
    
    // Number of states in the automata
    int                   num_states_ip = num_nodes + 1;
    int                   num_states_tr = 1 + (num_nodes * this->trace_width);
    
    // Initial states of the respective automata
    int                   s_init_ip = num_states_ip - 1;
    int                   s_init_tr = num_states_tr - 1;
    
    // References to the automata objects themselves
    spot::twa_graph_ptr   aut_ipath;
    spot::twa_graph_ptr   aut_trace;

    /*************************
     * Initialise the automata
     *************************/

    dict = spot::make_bdd_dict();
    aut_ipath = make_twa_graph(dict);
    aut_trace = make_twa_graph(dict);

    aut_ipath->set_buchi();
    aut_ipath->new_states(num_states_ip);
    aut_ipath->set_init_state(s_init_ip);

    aut_trace->set_buchi();
    aut_trace->new_states(num_states_tr);
    aut_trace->set_init_state(s_init_tr);

    /******************************
     * Set up the automata alphabet
     ******************************/

    Vec<bdd> idx_encoding(num_nodes, bddtrue);
    init_alphabet(num_nodes, aut_ipath, aut_trace, idx_encoding);


    /*********************************
     * Create the automata transitions
     *********************************/

    //
    // Path automaton
    //

    // Set up transitions corresponding to graph edges
    for (int src = 0; src < this->max_nodes; src++) {
        for (int sink = 0; sink < this->max_nodes; sink++) {
            if (h_change[src][sink] != NULL) {
                aut_ipath->new_edge(src, sink, idx_encoding[sink], {0});
            } else {
                // There should not be any edges between "non-existent" nodes
                assert(src < num_nodes && sink < num_nodes);
            }
        }
    }

    // Set up transitions from the initial state to the states corresponding
    // to graph nodes
    for (int sink = 0; sink < num_nodes; sink++) {
        aut_ipath->new_edge(s_init_ip, sink, idx_encoding[sink], {0});
    }

    //
    // Path automaton
    //

    // Any node takes init state to itself, with non-accepting transition
    aut_trace->new_edge(s_init_tr, s_init_tr, bddtrue);

    // The init state moves to every (node, height) state, with a non-accepting
    // transition recognising the node
    for (int n = 0; n < num_nodes; n++) {
        for (int h = 0; h < this->trace_width; h++) {
            aut_trace->new_edge(
                    s_init_tr,
                    (n * this->trace_width) + h,
                    idx_encoding[n]
                );
        }
    }

    // Now mirror the transitions in the underlying heighted graph
    for (int src = 0; src < num_nodes; src++) {
    for (int sink = 0; sink < num_nodes; sink++) {
        if (h_change[src][sink] != NULL) {
            for (int h_src = 0; h_src < this->trace_width; h_src++) {
            for (int h_sink = 0; h_sink < this->trace_width; h_sink++) {
                slope s = h_change[src][sink]->get_slope(h_src, h_sink);
                if (s == Stay) {
                    aut_trace->new_edge(
                            (src * this->trace_width) + h_src,
                            (sink * this->trace_width) + h_sink,
                            idx_encoding[sink]
                        );
                } else if (s == Downward) {
                    aut_trace->new_edge(
                            (src * this->trace_width) + h_src,
                            (sink * this->trace_width) + h_sink,
                            idx_encoding[sink],
                            {0}
                        );
                }
            }
            }
        }
    }
    }

    /*******************************
     * Perform the containment check
     *******************************/
    return spot::contains(aut_trace, aut_ipath);

}

bool Heighted_graph::sla_automata_check() {

    // BDD dictionary for the automata
    spot::bdd_dict_ptr    dict;
    
    // Number of vertices in the graph
    int                   num_nodes = this->num_nodes();
    
    // Number of states in the automata
    int                   num_states_ip = num_nodes + 1;
    int                   num_states_tr = this->trace_width + 1;

    // Initial states of the respective automata
    int                   s_init_ip = num_states_ip - 1;
    int                   s_init_tr = num_states_tr - 1;
    
    // References to the automata objects themselves
    spot::twa_graph_ptr   aut_ipath;
    spot::twa_graph_ptr   aut_trace;

    // A vector of the unique sloped relations occurring in the graph
    Vec<Sloped_relation*> relation_vec;
    // Mapping from edges to unique ID of the sloped relation along the edge
    Map<Int_pair,int>     relation_idx;

    // Construct unique IDs for each distinct sloped relation in the graph
    for (int src = 0; src < max_nodes; src++) {
        for (int sink = 0; sink < max_nodes; sink++) {
            if (h_change[src][sink] == NULL) continue;
            assert(src < num_nodes && sink < num_nodes);
            Sloped_relation* P = h_change[src][sink];
            P->initialize();
            int idx = 0;
            bool found = false;
            for (auto it = relation_vec.begin(); it != relation_vec.end(); it++)
            {
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
    aut_ipath->new_states(num_states_ip);
    aut_ipath->set_init_state(s_init_ip);

    aut_trace->set_buchi();
    aut_trace->new_states(num_states_tr);
    aut_trace->set_init_state(s_init_tr);


    /******************************
     * Set up the automata alphabet
     ******************************/

    Vec<bdd> idx_encoding(relation_vec.size(), bddtrue);
    // After calling the method to initialise the alphabet, the BDDs in the
    // [idx_encoding] vector are the alphabet letters representing the sloped
    // relations at the corresponding indices of the [relation_vec] vector
    init_alphabet(relation_vec.size(), aut_ipath, aut_trace, idx_encoding);


    /*********************************
     * Create the automata transitions
     *********************************/

    //
    // Path automaton
    //

    // Set up transitions corresponding to graph edges
    for (int src = 0; src < num_nodes; src++) {
        for (int sink = 0; sink < num_nodes; sink++) {
            if (h_change[src][sink] != NULL) {
                bdd curr_bdd = idx_encoding[relation_idx.at(Int_pair(src,sink))];
                aut_ipath->new_edge(src, sink, curr_bdd, {0});
            }
        }
    }

    // Set up transitions from the initial state to the states corresponding
    // to graph nodes
    for (int sink = 0; sink < num_nodes; sink++) {
        bdd curr_bdd = bddfalse;
        for (int src = 0; src < num_nodes; src++) {
            if (h_change[src][sink] != NULL) {
                curr_bdd |= idx_encoding[relation_idx.at(Int_pair(src,sink))];
            }
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
    for (int h1 = 0; h1 < this->trace_width; h1++) {
        for (int h2 = 0; h2 < this->trace_width; h2++) {

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