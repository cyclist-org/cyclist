#include "criterion.trace_manifold.hpp"
#include "bitset"
#include <vector>
#include <algorithm>
#include <cmath>
#include <cstring> // for memset!

void TraceManifoldCriterion::can_extend_path_to_submanifold_from(
    Int_pair trace_node,
    const Vec_shared_ptr<int> cycles_subset,
    Heighted_graph::TraceManifoldGraph trace_manifold_graph,
    Vec_shared_ptr<Int_pair> structural_connectivity_relation,
    Vec<bool> &is_cycle_visited,
    Vec<Int_pair> &visited_trace_nodes)
{
    // bool all_subset_is_visited = true;
    // for (int cycle_idx : *cycles_subset)
    // {
    //     all_subset_is_visited = all_subset_is_visited && is_cycle_visited[cycle_idx];
    // }
    // if (all_subset_is_visited)
    // {
    //     return;
    // }

    int cycle_idx = trace_node.first;
    is_cycle_visited[cycle_idx] = true;
    visited_trace_nodes.push_back(trace_node);

    for (const auto &[edge_first, edge_second] : *trace_manifold_graph.edges)
    {
        if (edge_first == trace_node || edge_second == trace_node)
        {
            Int_pair neighbour = edge_first == trace_node ? edge_second : edge_first;
            if (std::find(visited_trace_nodes.begin(), visited_trace_nodes.end(), neighbour) == visited_trace_nodes.end())
            {
                this->can_extend_path_to_submanifold_from(neighbour, cycles_subset, trace_manifold_graph, structural_connectivity_relation, is_cycle_visited, visited_trace_nodes);
            }
        }
    }
    // return false;
}

bool TraceManifoldCriterion::has_submanifold(
    const Vec_shared_ptr<int> &cycles_subset,
    Heighted_graph::TraceManifoldGraph trace_manifold_graph,
    Vec_shared_ptr<Int_pair> structural_connectivity_relation)
{
    int first_node_in_set = cycles_subset->at(0);
    Vec<Int_pair> visited_trace_nodes;
    Vec<bool> is_cycle_visited(trace_manifold_graph.trace_node_per_cycle->size());
    Vec<Int_pair> curr_path;

    for (auto &trace_node : *(trace_manifold_graph.trace_node_per_cycle->at(first_node_in_set)))
    {
        std::fill(is_cycle_visited.begin(), is_cycle_visited.end(), false);
        visited_trace_nodes.clear();

        can_extend_path_to_submanifold_from(trace_node, cycles_subset, trace_manifold_graph, structural_connectivity_relation, is_cycle_visited, visited_trace_nodes);

        bool are_all_cycles_in_subset_visited = true;
        for (int cycle_idx : *cycles_subset)
        {
            are_all_cycles_in_subset_visited = are_all_cycles_in_subset_visited && is_cycle_visited[cycle_idx];
        }
        if (are_all_cycles_in_subset_visited)
        {
            if (this->every_two_structurly_adjacent_cycles_have_an_edge_between_visited_traces(cycles_subset, visited_trace_nodes, structural_connectivity_relation, trace_manifold_graph.edges) &&
                this->exists_progressing_trace_node(visited_trace_nodes, cycles_subset, trace_manifold_graph.progressing_trace_nodes))
            {
                return true;
            }
        }
        curr_path.clear();
    }
    return false;
}

Vec_shared_ptr<int> TraceManifoldCriterion::subset_idxs_to_vec(
    Vec_shared_ptr<int> weakly_structurally_connected_component, size_t subset_idx)
{
    Vec_shared_ptr<int> result = std::make_shared<Vec<int>>();
    int curr_idx = 0;
    while (subset_idx != 0)
    {
        if ((subset_idx & 1) == 1)
        {
            result->push_back(weakly_structurally_connected_component->at(curr_idx));
        }
        subset_idx >>= 1;
        curr_idx++;
    }
    return result;
}

bool TraceManifoldCriterion::every_two_structurly_adjacent_cycles_have_an_edge_between_visited_traces(
    const Vec_shared_ptr<int> cycles_subset,
    const Vec<Int_pair> &visited_trace_nodes,
    Vec_shared_ptr<Int_pair> structural_connectivity_relation,
    Vec_shared_ptr<Pair<Int_pair, Int_pair>> trace_manifold_graph_edges)
{
    for (const auto &cycle1 : *cycles_subset)
    {
        for (const auto &cycle2 : *cycles_subset)
        {
            if (cycle1 == cycle2)
            {
                continue;
            }
            if (this->are_structurly_adjacent(cycle1, cycle2, structural_connectivity_relation) &&
                !this->are_visited_traces_graph_adjacent(cycle1, cycle2, visited_trace_nodes, trace_manifold_graph_edges))
            {
                return false;
            }
        }
    }
    return true;
}

bool TraceManifoldCriterion::are_structurly_adjacent(
    int cycle1,
    int cycle2,
    Vec_shared_ptr<Int_pair> structural_connectivity_relation)
{
    return std::find(
               structural_connectivity_relation->begin(),
               structural_connectivity_relation->end(),
               Int_pair(cycle1, cycle2)) != structural_connectivity_relation->end();
}

bool TraceManifoldCriterion::are_graph_adjacent(
    int cycle1,
    int cycle2,
    Vec_shared_ptr<Pair<Int_pair, Int_pair>> trace_manifold_graph_edges)
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

class CycleInSubsetNotVisited : public std::runtime_error
{
public:
    CycleInSubsetNotVisited() : std::runtime_error("cycle in subset was not visited while searching for submanifold") {}
};
bool TraceManifoldCriterion::are_visited_traces_graph_adjacent(
    int cycle1,
    int cycle2,
    const Vec<Int_pair> &visited_trace_nodes,
    Vec_shared_ptr<Pair<Int_pair, Int_pair>> trace_manifold_graph_edges)
{
    Vec<Int_pair> cycle1_trace_nodes, cycle2_trace_nodes;
    for (const auto visited_trace_node : visited_trace_nodes)
    {
        if (visited_trace_node.first == cycle1)
        {
            cycle1_trace_nodes.push_back( visited_trace_node);
        }
        if (visited_trace_node.first == cycle2)
        {
            cycle2_trace_nodes.push_back( visited_trace_node);
        }
    }
    
    for (const auto &[_,cycle1_trace_node] : cycle1_trace_nodes) {
    for (const auto &[_,cycle2_trace_node] : cycle2_trace_nodes) {
    for (const auto &[src, dest] : *trace_manifold_graph_edges)
    {
        if ((src.second == cycle1_trace_node) && (dest.second == cycle2_trace_node))
        {
            return true;
        }
    }
    }
    }

    return false;
}

bool TraceManifoldCriterion::exists_progressing_trace_node(
    Vec<Int_pair> &path,
    const Vec_shared_ptr<int> &cycles_subset,
    Set_shared_ptr<Int_pair> progressing_trace_nodes)
{
    for (const auto &trace_node : path)
    {
        if (progressing_trace_nodes->find(trace_node) != progressing_trace_nodes->end() &&
            std::find(cycles_subset->begin(), cycles_subset->end(), trace_node.first) != cycles_subset->end())
        {
            return true;
        }
    }
    return false;
}

void TraceManifoldCriterion::find_weakly_connected_component_of(
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

Vec_shared_ptr<Vec_shared_ptr<int>> TraceManifoldCriterion::calculate_weakly_connected_components(
    Heighted_graph::StructuralConnectivityRelation structural_connectivity_relation)
{
    Vec_shared_ptr<Vec_shared_ptr<int>> wccs = std::make_shared<Vec<Vec_shared_ptr<int>>>();
    int amount_of_cycles = structural_connectivity_relation.cycles->size();
    bool is_visited[amount_of_cycles];
    memset(is_visited, false, amount_of_cycles);
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

void TraceManifoldCriterion::traverse_cycles_subset_from(
    int cycle_idx,
    Vec<int> &visited_idxs,
    Vec_shared_ptr<int> cycles_subset,
    Vec_shared_ptr<Int_pair> structural_connectivity_relation)
{
    visited_idxs.push_back(cycle_idx);
    for (const auto &[edge_src, edge_dest] : *structural_connectivity_relation)
    {
        if (edge_src != cycle_idx && edge_dest != cycle_idx)
        {
            continue;
        }
        int neighbour_cycle_idx = edge_src == cycle_idx ? edge_dest : edge_src;
        if (std::find(cycles_subset->begin(), cycles_subset->end(), neighbour_cycle_idx) == cycles_subset->end())
        {
            continue;
        }
        if (std::find(visited_idxs.begin(), visited_idxs.end(), neighbour_cycle_idx) != visited_idxs.end())
        {
            continue;
        }
        traverse_cycles_subset_from(neighbour_cycle_idx, visited_idxs, cycles_subset, structural_connectivity_relation);
    }
}

bool TraceManifoldCriterion::is_connected_set(
    Vec_shared_ptr<int> cycles_subset,
    Vec_shared_ptr<Int_pair> structural_connectivity_relation)
{
    int amount_of_cycles_in_subset = cycles_subset->size();
    // printf("first cycle: %d\n", cycles_subset->at(0));
    // bool is_visited[amount_of_cycles_in_subset];
    // memset(is_visited, false, amount_of_cycles_in_subset);
    Vec<int> visited_cycles_idxs;
    this->traverse_cycles_subset_from(cycles_subset->at(0), visited_cycles_idxs, cycles_subset, structural_connectivity_relation);
    return visited_cycles_idxs.size() == cycles_subset->size();
    // for (int cycle_idx = 0; cycle_idx < amount_of_cycles_in_subset; cycle_idx++)
    // {
    //     if (!is_visited[cycle_idx])
    //     {
    //         printf("not visited!!!\n");
    //         return false;
    //     }
    // }
    // return true;
}

TraceManifoldCriterion::TraceManifoldCriterion(Heighted_graph *hg)
{
    this->hg = hg;
}

TraceManifoldCriterion::~TraceManifoldCriterion() {}

SoundnessCheckResult TraceManifoldCriterion::check_soundness()
{
    Heighted_graph::StructuralConnectivityRelation structural_connectivity_relation = this->hg->get_structural_connectivity_relation();
    if (!structural_connectivity_relation.is_cyclic_normal_form)
    {
        return SoundnessCheckResult::dontKnow;
    }
    Heighted_graph::TraceManifoldGraph trace_manifold_graph = this->hg->get_trace_manifold_graph(structural_connectivity_relation);

    //////////////////////////////////
    // printf("relation: \n");
    // for (const auto &[first, second] : *structural_connectivity_relation.relation)
    // {
    //     printf("(%d,%d), ", first, second);
    // }
    // printf("\n");
    // printf("amount of cycles: %d\n", structural_connectivity_relation.cycles->size());
    // printf("cycles: \n");
    // for (const auto &c : *structural_connectivity_relation.cycles)
    // {
    //     printf("cycle: ");
    //     for (int n : *c)
    //     {
    //         printf("%d,", n);
    //     }
    //     printf("\n");
    // }

    // printf("progressing traces: \n");
    // for (const auto &pt : *trace_manifold_graph.progressing_trace_nodes)
    // {
    //     printf("progressing: %d,%d\n", pt.first, pt.second);
    // }
    // printf("tmg edges %d\n", trace_manifold_graph.edges->size());

    // printf("trace manifold graph edges:\n");
    // for (const auto &[src, dest] : *trace_manifold_graph.edges)
    // {
    //     printf("(%d,%d)->(%d,%d)\n", src.first, src.second, dest.first, dest.second);
    // }

    // printf("trace manifold graph traces:\n");
    // for (const auto &traces_of_cycle : *trace_manifold_graph.trace_node_per_cycle) {
    // for (const auto &[node, height] : *traces_of_cycle) {
    //     printf("(%d,%d),", node, height);
    // }
    // printf("\n");
    // }

    //////////////////////////////////

    const Vec_shared_ptr<Vec_shared_ptr<int>> weakly_structurally_connected_components = this->calculate_weakly_connected_components(structural_connectivity_relation);
    for (auto weakly_structurally_connected_component : *weakly_structurally_connected_components)
    {
        for (size_t subset_idxs = 1; subset_idxs < std::pow(2, weakly_structurally_connected_component->size()); subset_idxs++)
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