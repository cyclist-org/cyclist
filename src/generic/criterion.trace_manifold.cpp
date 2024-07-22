#include "types.c"
#include "criterion.soundness.hpp"
#include "heighted_graph.hpp"
#include "bitset"

class TraceManifoldCriterion : public SoundnessCriterion
{
private:
    Heighted_graph *hg;

    bool can_extend_path_to_submanifold_from(
        Int_pair trace_node,
        const Vec_shared_ptr<int> cycles_subset,
        Heighted_graph::TraceManifoldGraph trace_manifold_graph,
        Vec_shared_ptr<Int_pair> structural_connectivity_relation,
        Vec<bool> is_cycle_visited,
        Vec<Int_pair> &curr_path)
    {
        if (curr_path.size() == cycles_subset->size())
        {
            bool are_all_cycles_in_subset_visited = false;
            for (const auto &is_curr_cycle_visted : is_cycle_visited)
            {
                are_all_cycles_in_subset_visited = are_all_cycles_in_subset_visited || is_curr_cycle_visted;
            }
            if (are_all_cycles_in_subset_visited &&
                this->every_two_structurly_adjacent_cycles_have_an_edge(cycles_subset, structural_connectivity_relation, trace_manifold_graph.edges) &&
                this->exists_progressing_trace_node(curr_path, trace_manifold_graph.progressing_trace_nodes))
            {
                return true;
            }
        }
        return false;

        int cycle_idx = trace_node.first;
        is_cycle_visited[cycle_idx] = true;

        for (const auto &[edge_first, edge_second] : *trace_manifold_graph.edges)
        {
            if (edge_first == trace_node || edge_second == trace_node)
            {
                Int_pair neighbour = edge_first == trace_node ? edge_second : edge_first;
                if (!is_cycle_visited[neighbour.first])
                {
                    // TODO
                }
            }
        }
    }

    bool
    has_submanifold(
        const Vec_shared_ptr<int> &cycles_subset,
        Heighted_graph::TraceManifoldGraph trace_manifold_graph,
        Vec_shared_ptr<Int_pair> structural_connectivity_relation)
    {
        int first_node_in_set = cycles_subset->at(0);
        Vec<bool> is_cycle_visited(cycles_subset->size());
        Vec<Int_pair> curr_path;

        for (auto &trace_node : *(trace_manifold_graph.trace_node_per_cycle->at(first_node_in_set)))
        {
            if (this->can_extend_path_to_submanifold_from(trace_node, cycles_subset, trace_manifold_graph, structural_connectivity_relation, is_cycle_visited, curr_path))
            {
                return true;
            };
        }
    }

    Vec_shared_ptr<int> subset_idxs_to_vec(Vec_shared_ptr<int> weakly_structurally_connected_component, size_t subset_idx)
    {
        Vec_shared_ptr<int> result = std::make_shared<Vec<int>>();
        int curr_idx = 0;
        while (subset_idx != 0)
        {
            if (subset_idx & 1 == 1)
            {
                result->push_back(weakly_structurally_connected_component->at(curr_idx));
            }
            subset_idx >>= 1;
        }
        return result;
    }

    bool every_two_structurly_adjacent_cycles_have_an_edge(const Vec_shared_ptr<int> cycles_subset, Vec_shared_ptr<Int_pair> structural_connectivity_relation, Vec_shared_ptr<Pair<Int_pair, Int_pair>> trace_manifold_graph_edges)
    {
        for (const auto &cycle1 : *cycles_subset)
        {
            for (const auto &cycle2 : *cycles_subset)
            {
                if (this->are_structurly_adjacent(cycle1, cycle2, structural_connectivity_relation) &&
                    !this->are_graph_adjacent(cycle1, cycle2, trace_manifold_graph_edges))
                {
                    return false;
                }
            }
        }
        return true;
    }

    bool are_structurly_adjacent(int cycle1, int cycle2, Vec_shared_ptr<Int_pair> structural_connectivity_relation)
    {
        return std::find(
                   structural_connectivity_relation->begin(),
                   structural_connectivity_relation->end(),
                   Int_pair(cycle1, cycle2)) != structural_connectivity_relation->end();
    }

    bool are_graph_adjacent(int cycle1, int cycle2, Vec_shared_ptr<Pair<Int_pair, Int_pair>> trace_manifold_graph_edges)
    {
        for (const auto &[src, dest] : *trace_manifold_graph_edges)
        {
            if (src.first == cycle1 && dest.first == cycle2)
            {
                return true;
            }
        }
        return false;
    }

    bool exists_progressing_trace_node(Vec<Int_pair> &path, Set_shared_ptr<Int_pair> progressing_trace_nodes)
    {
        for (const auto &trace_node : path)
        {
            if (progressing_trace_nodes->find(trace_node) != progressing_trace_nodes->end())
            {
                return true;
            }
        }
        return false;
    }

    void find_weakly_connected_component_of(
        int cycle_idx,
        Vec_shared_ptr<Int_pair> structural_connectivity_relation,
        bool *is_visited,
        Vec_shared_ptr<int> wcc)
    {
        is_visited[cycle_idx] = true;
        if (wcc != nullptr)
        {
            wcc->push_back(cycle_idx);
        }
        for (const auto &[edge_src, edge_dest] : *structural_connectivity_relation)
        {
            if (edge_src != cycle_idx && edge_dest != cycle_idx)
            {
                continue;
            }
            int neighbour_cycle_idx = edge_src == cycle_idx ? edge_dest : edge_src;
            if (is_visited[neighbour_cycle_idx])
            {
                continue;
            }
            find_weakly_connected_component_of(neighbour_cycle_idx, structural_connectivity_relation, is_visited, wcc);
        }
    }

    Vec_shared_ptr<Vec_shared_ptr<int>> calculate_weakly_connected_components(Heighted_graph::StructuralConnectivityRelation structural_connectivity_relation)
    {
        Vec_shared_ptr<Vec_shared_ptr<int>> wccs = std::make_shared<Vec<Vec_shared_ptr<int>>>();
        int amount_of_cycles = structural_connectivity_relation.cycles->size();
        bool is_visited[amount_of_cycles];
        for (int cycle_idx = 0; cycle_idx < structural_connectivity_relation.cycles->size(); cycle_idx++)
        {
            if (is_visited[cycle_idx])
            {
                continue;
            }
            Vec_shared_ptr<int> wcc = std::make_shared<Vec<int>>();
            this->find_weakly_connected_component_of(cycle_idx, structural_connectivity_relation.relation, is_visited, wcc);
            wccs->push_back(wcc);
        }
        return wccs;
    }

    bool is_connected_set(Vec_shared_ptr<int> cycles_subset, Vec_shared_ptr<Int_pair> structural_connectivity_relation)
    {
        int amount_of_cycles_in_subset = cycles_subset->size();
        bool is_visited[amount_of_cycles_in_subset];
        this->find_weakly_connected_component_of(cycles_subset->at(0), structural_connectivity_relation, is_visited, nullptr);
        for (int cycle_idx = 0; cycle_idx < amount_of_cycles_in_subset; cycle_idx++)
        {
            if (!is_visited[cycle_idx])
            {
                return false;
            }
        }
        return true;
    }

public:
    TraceManifoldCriterion(Heighted_graph *hg)
    {
        this->hg = hg;
    }
    ~TraceManifoldCriterion()
    {
    }

    SoundnessCheckResult check_soundness()
    {
        Heighted_graph::StructuralConnectivityRelation structural_connectivity_relation = this->hg->get_structural_connectivity_relation();
        Heighted_graph::TraceManifoldGraph trace_manifold_graph = this->hg->get_trace_manifold_graph(structural_connectivity_relation);

        const Vec_shared_ptr<Vec_shared_ptr<int>> weakly_structurally_connected_components = this->calculate_weakly_connected_components(structural_connectivity_relation);
        for (auto weakly_structurally_connected_component : *weakly_structurally_connected_components)
        {
            for (size_t subset_idxs = 0; subset_idxs < std::pow(2, weakly_structurally_connected_component->size()); subset_idxs++)
            {
                Vec_shared_ptr<int> cycles_subset = this->subset_idxs_to_vec(weakly_structurally_connected_component, subset_idxs);
                if (!this->is_connected_set(cycles_subset, structural_connectivity_relation.relation))
                {
                    continue;
                }
                if (!this->has_submanifold(cycles_subset, trace_manifold_graph, structural_connectivity_relation.relation))
                {
                    return SoundnessCheckResult::dontKnow;
                }
            }
        }
        return SoundnessCheckResult::sound;
    }
};

template <typename NodeType>
using GraphEdges = Map<NodeType, Vec<NodeType> *>;