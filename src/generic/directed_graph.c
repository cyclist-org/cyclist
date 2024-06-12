#include "directed_graph.hpp"
#include "types.c"

#include <stack>
#include <unordered_set>
#include <algorithm>

bool DirectedGraph::is_cycle_reachable_from(int node, Vec<int> *curr_path, bool *fresh_nodes)
{
    curr_path->push_back(node);
    auto node_neighbours = this->neighbours_map->at(node);
    for (int neighbour : *node_neighbours)
    {
        if (fresh_nodes[neighbour] == false)
        {
            continue;
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
        fresh_nodes[i] = true;
    }

    for (int node = 0; node < this->num_nodes; node++)
    {
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

int DirectedGraph::count_reachable_backedges(int node, Vec<int> *curr_path, bool *fresh_nodes)
{
    curr_path->push_back(node);
    auto node_neighbours = this->neighbours_map->at(node);
    int curr_backedges_count = 0;

    fresh_nodes[node] = false;

    for (int neighbour : *node_neighbours)
    {
        if (find(curr_path->begin(), curr_path->end(), neighbour) != curr_path->end())
        {
            fresh_nodes[node] = false;
            curr_backedges_count++;
        }
        if (fresh_nodes[neighbour] == false)
        {
            continue;
        }
        else
        {
            curr_backedges_count += count_reachable_backedges(neighbour, curr_path, fresh_nodes);
        }
    }
    curr_path->pop_back();
    return curr_backedges_count;
}

int DirectedGraph::count_backedges()
{
    bool fresh_nodes[this->num_nodes];
    for (int i = 0; i < this->num_nodes; i++)
    {
        fresh_nodes[i] = true;
    }

    int backedges_count = 0;
    for (int node = 0; node < this->num_nodes; node++)
    {
        if (fresh_nodes[node] == false)
        {
            continue;
        }
        Vec<int> visited;
        backedges_count += this->count_reachable_backedges(node, &visited, fresh_nodes);
    }
    return backedges_count;
}

void mark_in_cycle_from(Vec<int> *curr_path, int first_node_in_cycle, bool *is_in_cycle)
{
    for (int i = curr_path->size() - 1; i >= 0; i--)
    {
        int curr_node = (*curr_path)[i];
        is_in_cycle[curr_node] = true;
        if (curr_node == first_node_in_cycle)
        {
            break;
        }
    }
}

bool DirectedGraph::is_overlapping_cycle_reachable_from(int node, std::stack<int> &s, bool *is_on_stack, int *idxs, int *low_links, int &next_idx, std::vector<int> &backedge_dests)
{
    idxs[node] = next_idx;
    low_links[node] = next_idx;
    next_idx++;
    s.push(node);
    is_on_stack[node] = true;

    auto node_neighbours = this->neighbours_map->at(node);

    for (int neighbour : *node_neighbours)
    {
        if (idxs[neighbour] == -1)
        {
            if (is_overlapping_cycle_reachable_from(neighbour, s, is_on_stack, idxs, low_links, next_idx, backedge_dests))
            {
                return true;
            }
            low_links[node] = std::min(low_links[node], low_links[neighbour]);
        }
        else if (is_on_stack[neighbour])
        {
            low_links[node] = std::min(low_links[node], idxs[neighbour]);
            backedge_dests.push_back(neighbour);
        }
    }

    if (low_links[node] == idxs[node])
    {
        int curr_on_scc = -1;
        while (curr_on_scc != node)
        {
            curr_on_scc = s.top();
            s.pop();
            is_on_stack[curr_on_scc] = false;
        }
    }
}

bool DirectedGraph::contains_overlapping_cycles()
{
    int next_index = 0;
    std::stack<int> s;
    std::vector<int> backedge_dests;

    int idxs[this->num_nodes];
    int low_links[this->num_nodes];
    bool is_on_stack[this->num_nodes];

    for (size_t i = 0; i < this->num_nodes; i++)
    {
        idxs[i] = -1;
        low_links[i] = -1;
        is_on_stack[i] = false;
    }

    for (int node = 0; node < this->num_nodes; node++)
    {
        if (idxs[node] == -1)
        {
            if (this->is_overlapping_cycle_reachable_from(node, s, is_on_stack, idxs, low_links, next_index, backedge_dests))
            {
                return true;
            }
        }
    }

    std::unordered_set<int> backedge_dests_low_links;
    for (int backedge_dest_node : backedge_dests)
    {
        int low_link = low_links[backedge_dest_node];
        if (backedge_dests_low_links.find(low_link) != backedge_dests_low_links.end())
        {
            return true;
        }
        backedge_dests_low_links.insert(low_link);
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