#include "types.c"

class DirectedGraph
{
private:
    Map<int, Int_SET *> *neighbours_map;

    bool is_cycle_reachable_from(int node, Vec<int> *visited, Int_SET *fresh_nodes)
    {
        fresh_nodes->erase(node);
        auto node_neighbours = this->neighbours_map->at(node);
        for (int neighbour : *node_neighbours)
        {
            if (find(visited->begin(), visited->end(), neighbour) != visited->end())
            {
                return true;
            }
            visited->push_back(neighbour);
            if (is_cycle_reachable_from(neighbour, visited, fresh_nodes))
            {
                return true;
            }
        }
        if (node_neighbours->size() == 0)
        {
            visited->pop_back();
        }
        return false;
    }

public:
    DirectedGraph() {}
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
            fresh_nodes.insert(node_neighbours.first);
        }

        while (fresh_nodes.size() > 0)
        {
            int node = *fresh_nodes.begin();
            fresh_nodes.erase(node);
            Vec<int> visited({node});
            if (this->is_cycle_reachable_from(node, &visited, &fresh_nodes))
            {
                return true;
            }
        }
        return false;
    }
};