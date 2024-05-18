#pragma once

#include "types.c"

class DirectedGraph
{
private:
    Map<int, Int_SET *> *neighbours_map;
    int num_nodes;
    bool *should_halt;

    bool is_cycle_reachable_from(int node, Vec<int> *curr_path, bool *fresh_nodes);

public:
    DirectedGraph(bool *should_halt);
    DirectedGraph(Map<int, Int_SET *> *neighbours_map, int num_nodes);
    ~DirectedGraph();

    bool contains_cycle();
    Vec<Vec<Int_pair> *> *get_elementary_cycles();
    void print();
};