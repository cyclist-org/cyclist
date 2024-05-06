#pragma once

#include "types.c"

class DirectedGraph
{
private:
    Map<int, Int_SET *> *neighbours_map;
    bool *should_halt;

    bool is_cycle_reachable_from(int node, Vec<int> *curr_path, Int_SET *fresh_nodes);

public:
    DirectedGraph(bool *should_halt);
    DirectedGraph(Map<int, Int_SET *> *neighbours_map);
    ~DirectedGraph();

    bool contains_cycle();
    void print();
};