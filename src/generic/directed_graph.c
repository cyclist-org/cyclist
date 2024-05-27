#include "directed_graph.hpp"
#include "types.c"

#include <algorithm>

bool DirectedGraph::is_cycle_reachable_from(int node, Vec<int> *curr_path, bool* fresh_nodes)
{
    curr_path->push_back(node);
    auto node_neighbours = this->neighbours_map->at(node);
    for (int neighbour : *node_neighbours)
    {
        if (fresh_nodes[neighbour] == false)
        {
            continue;
        }
        if (this->should_halt)
        {
            fresh_nodes[node] = false;
            return false;
        }
        if (find(curr_path->begin(), curr_path->end(), neighbour) != curr_path->end())
        {
            fresh_nodes[node] = false;
            return true;
        }
        if (is_cycle_reachable_from(neighbour, curr_path, fresh_nodes))
        {
            fresh_nodes[node] = false;
            return true;
        }
    }
    fresh_nodes[node] = false;
    curr_path->pop_back();
    return false;
}

DirectedGraph::DirectedGraph(bool *should_halt) : should_halt(should_halt) {}
DirectedGraph::DirectedGraph(Map<int, Int_SET *> *neighbours_map, int num_nodes)
{
    this->neighbours_map = neighbours_map;
    this->num_nodes = num_nodes;
}
DirectedGraph::~DirectedGraph()
{
    for (auto node_neighbours : (*this->neighbours_map))
    {
        delete node_neighbours.second;
    }
    delete this->neighbours_map;
}

bool DirectedGraph::contains_cycle()
{
    bool fresh_nodes[this->num_nodes];
    for (int i = 0; i < this->num_nodes; i++)
    {
        if (this->should_halt)
        {
            return false;
        }
        fresh_nodes[i] = true;
    }

    for (int node = 0; node < this->num_nodes; node++)
    {
        if (this->should_halt)
        {
            return false;
        }
        if (fresh_nodes[node] == false)
        {
            continue;
        }
        Vec<int> visited;
        if (this->is_cycle_reachable_from(node, &visited, fresh_nodes))
        {
            return true;
        }
    }
    return false;
}

void output(Vec<int> *Stack, int V, Vec<Vec<Int_pair> *> *cycles)
{
    Vec<Int_pair> *cycle = new Vec<Int_pair>();
    int left = -1;
    Vec<int> idxs;
    for (auto right = Stack->begin(), E = Stack->end(); right != E; ++right)
    {
        idxs.push_back(*right);
        if (left != -1)
            cycle->push_back(Int_pair(left - 1, (*right) - 1));
        left = *right;
    }
    cycle->push_back(Int_pair(left - 1, *(Stack->begin()) - 1));
    cycles->push_back(cycle);
}

void unblock(Vec<NodeList> *B, Vec<bool> *Blocked, int U)
{
    Blocked->at(U - 1) = false;
    while (!B->at(U - 1).empty())
    {
        int W = B->at(U - 1).front();
        B->at(U - 1).pop_front();
        if (Blocked->at(W - 1))
        {
            unblock(B, Blocked, W);
        }
    }
}

bool circuit(Map<int, NodeList> *AK, Vec<NodeList> *B, Vec<int> *Stack, Vec<bool> *Blocked, int S, int V, Vec<Vec<Int_pair> *> *cycles)
{
    bool F = false;
    Stack->push_back(V);
    Blocked->at(V - 1) = true;

    for (int W : AK->at(V - 1))
    {
        if (W == S)
        {
            output(Stack, V, cycles);
            F = true;
        }
        else if (W > S && !Blocked->at(W - 1))
        {
            F = circuit(AK, B, Stack, Blocked, S, W, cycles);
        }
    }

    if (F)
    {
        unblock(B, Blocked, V);
    }
    else
    {
        for (int W : AK->at(V - 1))
        {
            auto IT = std::find((B->at(W - 1)).begin(), (B->at(W - 1)).end(), V);
            if (IT == (B->at(W - 1)).end())
            {
                (B->at(W - 1)).push_back(V);
            }
        }
    }

    Stack->pop_back();
    return F;
}

Vec<Vec<Int_pair> *> *DirectedGraph::get_elementary_cycles()
{
    Vec<Vec<Int_pair> *> *cycles = new Vec<Vec<Int_pair> *>();

    Map<int, NodeList> *AK = new Map<int, NodeList>();

    int max_node = -1;
    for (const auto &[v, neighbours] : *this->neighbours_map)
    {
        if (max_node < v)
        {
            max_node = v;
        }
        for (const auto &neighbour : *neighbours)
        {
            if (max_node < neighbour)
            {
                max_node = neighbour;
            }
            (*AK)[v].push_back(neighbour + 1);
        }
    }

    int N = max_node + 1;
    Vec<NodeList> *B = new Vec<NodeList>(N);
    Vec<int> *Stack = new Vec<int>();
    Vec<bool> *Blocked = new Vec<bool>(N);
    Stack->clear();
    int S = 1;

    while (S < N)
    {
        for (int I = S; I <= N; ++I)
        {
            Blocked->at(I - 1) = false;
            B->at(I - 1).clear();
        }
        circuit(AK, B, Stack, Blocked, S, S, cycles);
        ++S;
    }

    delete AK;
    delete B;
    delete Stack;
    delete Blocked;

    return cycles;
}

void DirectedGraph::print()
{
    for (const auto &[node, neighbours] : *(this->neighbours_map))
    {
        for (const auto &neighbour : *neighbours)
        {
            printf("%d->%d\n", node, neighbour);
        }
    }
}