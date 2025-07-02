#ifndef HEIGHTED_GRAPH_HH_
#define HEIGHTED_GRAPH_HH_

#include <chrono>
#include <iostream>
#include <fstream>
#include <set>
#include <utility>
#include <cassert>
#include <memory>
#include <iostream>
#include <sstream>
#include <cstdint>
#include <map>
#include <stack>

#include "sloped_relation.hpp"
#include "types.c"


#define DURATION std::milli

class Heighted_graph {

private:

    int                         max_nodes;
    int                         num_edges_;
    Map<int, int>               node_idxs;
    Vec<bool>                   is_node_a_bud;
    std::vector<Map<int, int>*> height_idxs;
    std::vector<Int_SET*>       HeightsOf;
    Sloped_relation***          h_change;

    // The maximum number of heights in a node
    int trace_width = 0;

    int flags;

#ifdef LOG_STATS
    int                     ccl_initial_size;
    int                     ccl_size;
    int                     ccl_iterations;
    int                     ccl_replacements;
    int                     ccl_rejections;
    int                     compositions;
    int                     comparisons;
    int                     loop_checks;
    int                     checked_size_sum;
    int                     total_size_sum;
    std::chrono::duration<double, DURATION> compose_time;
    std::chrono::duration<double, DURATION> compare_time;
    std::chrono::duration<double, DURATION> need_to_add_compute_time;
    std::chrono::duration<double, DURATION> insertion_time;
    std::chrono::duration<double, DURATION> loop_check_time;
#endif

    bool check_and_add(
            Relation_LIST& entry, Sloped_relation& R,
            Set<Sloped_relation*>& visited,
            Relation_LIST& preceded,
            Relation_LIST::iterator preserve_end, int opts);
    bool check_self_loop(Sloped_relation& R, int opts);
    bool check_Ccl(Relation_LIST*** ccl, int opts);
    bool check_Ccl(Set<Sloped_relation*>*** ccl, int opts);

    void find_scc_and_remove_down_edges_not_in_it(
        int n,
        std::stack<int>& s,
        bool* on_stack,
        int* idxs,
        int* low_links,
        int& next_idx,
        Vec<Int_pair>& extended_nodes,
        Map<Int_pair, int>& extended_nodes_idxs
    );
    void find_backedge_dests_and_SCCs_reachable_from(
        int node,
        std::stack<int> &s,
        bool *on_stack,
        int *idxs,
        int *low_links,
        int &next_idx,
        std::vector<int> &backedge_dests,
        std::vector<std::vector<int>*> &SCCs
    );
    bool is_down_extended_SCC_reachable_in_node_SCC_from(
        Vec<int>* node_SCC,
        Int_pair extended_node,
        std::stack<Int_pair> &s,
        std::stack<slope> &slopes_stack,
        Int_pair_SET &on_stack,
        std::map<Int_pair, int> &idxs,
        std::map<Int_pair, int> &low_links,
        int &next_idx
    );
    Vec<Int_pair> get_extended_nodes();

    void explore_traces_from(
        int height,
        Vec_shared_ptr<int> cycle,
        int cycle_idx,
        Vec<int>& curr_heights_path,
        bool does_curr_path_have_down_slope,
        Vec_shared_ptr<Vec_shared_ptr<int>> traces,
        Set_shared_ptr<Int_pair> progressing_traces
    );

    Vec_shared_ptr<Vec_shared_ptr<int>> get_traces_of_cycle(Vec_shared_ptr<int> cycle, int cycle_idx, Set_shared_ptr<Int_pair> progressing_traces);

    bool is_flat_cycle_reachable_from(int node, Vec<int>* curr_path, bool* fresh_nodes);

    bool explore_height(int edge_idx, Vec<int>& heights_stack, const Vec<Int_pair>& cycle, bool visited_down_slope);

    int find_root_node();
    void explore_basic_cycles_from(
        int node,
        Vec<int>& curr_path,
        bool* is_fresh,
        Vec_shared_ptr<int> companions,
        Vec_shared_ptr<Vec_shared_ptr<int>> cycles
    );

    void remove_hchange(int source_idx, int source_h_idx, int sink_idx, int sink_h_idx, slope s);

public:

    // Option flags
    static const int FAIL_FAST;
    static const int USE_TRANSITIVE_LOOPING;
    static const int USE_IDEMPOTENCE;
    static const int USE_MINIMALITY;

    static const int PRINT_CCL;

    enum NODE_ORDER {
        GIVEN_ORDER = 0,
        DEGREE_OUT_IN_ASC = 1,
        DEGREE_OUT_IN_DESC = 2
    };

    static int parse_flags(const std::string flags);
    static void print_flags(int flags);

    Heighted_graph(int max_nodes);
    ~Heighted_graph(void);

    static Heighted_graph* clone(Heighted_graph* hg);

    // Methods for constructing the height graph
    void add_node(int n, bool is_bud);
    void add_height(int n, int h);
    void add_edge(int source, int sink);
    void add_hchange(int source, int source_h, int sink, int sink_h, slope s);
    void add_stay(int source_node, int source_h, int sink_node, int sink_h);
    void add_decrease(int source_node, int source_h, int sink_node, int sink_h);

    void remove_down_edges_not_in_any_SCC();

    bool has_flat_cycle();

    bool calculate_SCCs_and_check_if_has_overlapping_cycles(Vec<Vec<int>*> &SCCs);
    bool does_node_SCC_contain_a_down_extended_SCC(Vec<int>* node_SCC);

    struct StructuralConnectivityRelation {
        Vec_shared_ptr<Vec_shared_ptr<int>> cycles;
        Vec_shared_ptr<int> companions;
        Vec_shared_ptr<Int_pair> relation;
        bool is_cyclic_normal_form;
    };
    StructuralConnectivityRelation get_structural_connectivity_relation();

    struct TraceManifoldGraph {
        Vec_shared_ptr<Vec_shared_ptr<Int_pair>> trace_node_per_cycle;
        Vec_shared_ptr<Pair<Int_pair, Int_pair>> edges;
        Set_shared_ptr<Int_pair> progressing_trace_nodes;
    };
    TraceManifoldGraph get_trace_manifold_graph(
        StructuralConnectivityRelation structural_connectivity_relation
    );

    bool is_cycle_oneshot(const Vec<Int_pair>& cycle);
    bool are_all_relations_partial_functions();

    bool has_self_edge(int node_idx);

    slope get_slope(int src, int sink ,int source_h, int sink_h);

    Map<int,Int_SET*> *get_edges_adjacency_list();
    Map<int,Int_SET*> *get_flat_edges();
    Map<Int_pair, Int_pair_SET*> *get_stay_extended_edges();
    Map<Int_pair, Int_pair_SET*> *get_extended_edges();

    Vec<Int_pair> *get_edges();
    int get_max_nodes();
    Sloped_relation*** get_h_change();
    std::vector<Int_SET*>* get_HeightsOf();
    Vec<Set<Int_pair>> get_SCCs();


    int num_nodes(void);
    int num_edges(void);

    static bool is_valid_node_order(int order);

    bool order_reduced_check(NODE_ORDER order, int opts);
    bool order_reduced_check(NODE_ORDER order, int opts, bool* should_halt);
    bool fwk_check(int opts);
    bool fwk_check(int opts, bool* should_halt);
    bool sla_automata_check(void);
    bool vla_automata_check(void);

    std::string to_string();

    void print_statistics(void);
};

#endif