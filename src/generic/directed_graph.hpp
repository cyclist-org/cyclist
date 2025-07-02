#pragma once

#include "types.c"
#include <stack>

class DirectedGraph
{
private:
    Map<int, Int_SET *> *neighbours_map;
    int num_nodes;
    bool *should_halt;

    bool is_cycle_reachable_from(int node, Vec<int> *curr_path, bool *fresh_nodes);
    // bool is_overlapping_cycle_reachable_from(int node, std::stack<int> &s, bool *is_on_stack, int *idxs, int *low_links, int next_idx, std::stack<int> & backedge_dests_stack);
    bool is_overlapping_cycle_reachable_from(int node, std::stack<int> &s, bool *is_on_stack, int *idxs, int *low_links, int &next_idx, std::vector<int> & backedge_dests);
    int count_reachable_backedges(int node, Vec<int> *curr_path, bool *fresh_nodes);

public:
    DirectedGraph(bool *should_halt);
    DirectedGraph(Map<int, Int_SET *> *neighbours_map, int num_nodes);
    ~DirectedGraph();

    bool contains_cycle();
    int count_backedges();
    bool contains_overlapping_cycles();
    Vec<Vec<Int_pair> *> *get_elementary_cycles();
    void print();
};