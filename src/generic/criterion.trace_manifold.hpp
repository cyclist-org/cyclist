#include "criterion.soundness.hpp"
#include "heighted_graph.hpp"

class TraceManifoldCriterion : public SoundnessCriterion
{
private:
    Heighted_graph *hg;

    void can_extend_path_to_submanifold_from(
        Int_pair trace_node,
        const Vec_shared_ptr<int> cycles_subset,
        Heighted_graph::TraceManifoldGraph trace_manifold_graph,
        Vec_shared_ptr<Int_pair> structural_connectivity_relation,
        Vec<bool> &is_cycle_visited,
        Vec<Int_pair> &visited_trace_nodes
    );

    bool has_submanifold(
        const Vec_shared_ptr<int> &cycles_subset,
        Heighted_graph::TraceManifoldGraph trace_manifold_graph,
        Vec_shared_ptr<Int_pair> structural_connectivity_relation
    );

    Vec_shared_ptr<int> subset_idxs_to_vec(
        Vec_shared_ptr<int> weakly_structurally_connected_component,
        size_t subset_idx
    );

    bool every_two_structurly_adjacent_cycles_have_an_edge_between_visited_traces(
        const Vec_shared_ptr<int> cycles_subset,
        const Vec<Int_pair> &visited_trace_nodes,
        Vec_shared_ptr<Int_pair> structural_connectivity_relation,
        Vec_shared_ptr<Pair<Int_pair, Int_pair>> trace_manifold_graph_edges
    );

    bool are_structurly_adjacent(
        int cycle1,
        int cycle2,
        Vec_shared_ptr<Int_pair> structural_connectivity_relation
    );
    

    bool are_graph_adjacent(
        int cycle1,
        int cycle2,
        Vec_shared_ptr<Pair<Int_pair, Int_pair>> trace_manifold_graph_edges
    );
    bool are_visited_traces_graph_adjacent(
        int cycle1,
        int cycle2,
        const Vec<Int_pair> &visited_trace_nodes,
        Vec_shared_ptr<Pair<Int_pair, Int_pair>> trace_manifold_graph_edges
    );

    bool exists_progressing_trace_node(
        Vec<Int_pair> &path,
        const Vec_shared_ptr<int> &cycles_subset,
        Set_shared_ptr<Int_pair> progressing_trace_nodes
    );

    void find_weakly_connected_component_of(
        int cycle_idx,
        Vec_shared_ptr<Int_pair> structural_connectivity_relation,
        bool *is_visited,
        Vec_shared_ptr<int> wcc
    );

    Vec_shared_ptr<Vec_shared_ptr<int>> calculate_weakly_connected_components(
        Heighted_graph::StructuralConnectivityRelation structural_connectivity_relation
    );

    void traverse_cycles_subset_from(
        int cycle_idx,
        Vec<int> &visited_idxs,
        Vec_shared_ptr<int> cycles_subset,
        Vec_shared_ptr<Int_pair> structural_connectivity_relation
    );

    bool is_connected_set(
        Vec_shared_ptr<int> cycles_subset,
        Vec_shared_ptr<Int_pair> structural_connectivity_relation
    );

public:
    TraceManifoldCriterion(Heighted_graph *hg);
    ~TraceManifoldCriterion();

    SoundnessCheckResult check_soundness();
};