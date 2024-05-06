#include "types.c"

class DirectedGraph
{
private:
    Map<int, Int_SET *> *neighbours_map;
    bool *should_halt;

    bool is_cycle_reachable_from(int node, Vec<int> *curr_path, Int_SET *fresh_nodes)
    {
        curr_path->push_back(node);
        auto node_neighbours = this->neighbours_map->at(node);
        for (int neighbour : *node_neighbours)
        {
            if (fresh_nodes->find(neighbour) == fresh_nodes->end())
            {
                continue;
            }
            if (this->should_halt)
            {
                fresh_nodes->erase(node);
                return false;
            }
            if (find(curr_path->begin(), curr_path->end(), neighbour) != curr_path->end())
            {
                fresh_nodes->erase(node);
                return true;
            }
            // curr_path->push_back(neighbour);
            if (is_cycle_reachable_from(neighbour, curr_path, fresh_nodes))
            {
                fresh_nodes->erase(node);
                return true;
            }
        }
        fresh_nodes->erase(node);
        curr_path->pop_back();
        return false;
    }

public:
    DirectedGraph(bool *should_halt) : should_halt(should_halt) {}
    DirectedGraph(Map<int, Int_SET *> *neighbours_map)
    {
        this->neighbours_map = neighbours_map;
    }
    ~DirectedGraph()
    {
        for (auto node_neighbours : (*this->neighbours_map))
        {
            delete node_neighbours.second;
        }
        delete this->neighbours_map;
    }

    bool contains_cycle()
    {
        Int_SET fresh_nodes;
        for (auto node_neighbours : (*this->neighbours_map))
        {
            if (this->should_halt)
            {
                return false;
            }
            fresh_nodes.insert(node_neighbours.first);
        }

        while (fresh_nodes.size() > 0)
        {
            if (this->should_halt)
            {
                return false;
            }
            int node = *fresh_nodes.begin();
            // fresh_nodes.erase(node);
            // Vec<int> visited({node});
            Vec<int> visited;
            if (this->is_cycle_reachable_from(node, &visited, &fresh_nodes))
            {
                return true;
            }
        }
        return false;
    }

    void print()
    {
        for (const auto &[node, neighbours] : *(this->neighbours_map))
        {
            for (const auto &neighbour : *neighbours)
            {
                printf("%d->%d\n", node, neighbour);
            }
        }
    }
};