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

//======================================================
#include <spot/tl/defaultenv.hh>

#include <spot/twa/bdddict.hh>
#include <spot/twa/twa.hh>
#include <spot/twa/twagraph.hh>
#include <spot/twa/twaproduct.hh>

#include <spot/twaalgos/contains.hh>
#include <spot/twaalgos/copy.hh>
#include <spot/twaalgos/determinize.hh>
#include <spot/twaalgos/dot.hh>
#include <spot/twaalgos/dualize.hh>
#include <spot/twaalgos/hoa.hh>
#include <spot/twaalgos/stutter.hh>
#include <spot/twaalgos/totgba.hh>
#include <spot/twaalgos/remfin.hh>

#include <spot/twaalgos/gtec/gtec.hh>

#include "sloped_relation.hpp"
#include "types.c"


#define DURATION std::milli

class Heighted_graph {

private:

    int                         max_nodes;
    int                         num_edges_;
    Map<int, int>               node_idxs;
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


public:

    // Option flags
    static const int FAIL_FAST;
    static const int USE_SCC_CHECK;
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
    void add_node(int n);
    void add_height(int n, int h);
    void add_edge(int source, int sink);
    void add_hchange(int source, int source_h, int sink, int sink_h, slope s);
    void add_stay(int source_node, int source_h, int sink_node, int sink_h);
    void add_decrease(int source_node, int source_h, int sink_node, int sink_h);

    slope get_slope(int src, int sink ,int source_h, int sink_h);

    int num_nodes(void);
    int num_edges(void);

    static bool is_valid_node_order(int order);

    bool order_reduced_check(NODE_ORDER order, int opts);
    bool order_reduced_check(NODE_ORDER order, int opts, bool* should_halt);
    bool fwk_check(int opts);
    bool fwk_check(int opts, bool* should_halt);
    bool sla_automata_check(void);
    bool vla_automata_check(void);

    void print_statistics(void);

};

#endif