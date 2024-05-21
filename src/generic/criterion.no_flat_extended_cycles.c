#include "types.c"
#include "criterion.soundness.hpp"
#include "heighted_graph.hpp"
#include "directed_graph.hpp"

class NoFlatExtendedCyclesCriterion : public SoundnessCriterion
{
private:
    Heighted_graph *hg;
    DirectedGraph *dg;
    Map<int, Int_pair> index_to_extended_node;

    bool does_cycle_contain_down_slope(Vec<Int_pair> *cycle)
    {
        for (const auto &[src_idx, sink_idx] : *cycle)
        {
            const auto [src_node_idx, src_height_idx] = this->index_to_extended_node.at(src_idx);
            const auto [sink_node_idx, sink_height_idx] = this->index_to_extended_node.at(sink_idx);

            slope slp = this->hg->get_slope(src_node_idx, sink_node_idx, src_height_idx, sink_height_idx);

            if (slp == slope::Downward)
            {
                return true;
            }
        }

        return false;
    }

public:
    NoFlatExtendedCyclesCriterion(Heighted_graph *hg)
    {
        this->hg = hg;
        Map<Int_pair, Int_pair_SET *> *extended_stay_edges = this->hg->get_stay_extended_edges();
        Map<Int_pair, int> extended_nodes_to_indices;
        Map<int, Int_SET *> *extended_indexed_stay_edges = new Map<int, Int_SET *>();

        for (const auto &[extended_node, neighbours] : (*extended_stay_edges))
        {
            if (extended_nodes_to_indices.find(extended_node) == extended_nodes_to_indices.end())
            {
                int extended_node_idx = extended_nodes_to_indices.size();
                extended_nodes_to_indices[extended_node] = extended_node_idx;
                (*extended_indexed_stay_edges)[extended_node_idx] = new Int_SET();
            }
            int extended_node_idx = extended_nodes_to_indices[extended_node];
            for (const auto &neighbour_extended_node : *neighbours)
            {
                if (extended_nodes_to_indices.find(neighbour_extended_node) == extended_nodes_to_indices.end())
                {
                    int neighbour_extended_node_idx = extended_nodes_to_indices.size();
                    extended_nodes_to_indices[neighbour_extended_node] = neighbour_extended_node_idx;
                    (*extended_indexed_stay_edges)[neighbour_extended_node_idx] = new Int_SET();
                }
                (*extended_indexed_stay_edges)[extended_node_idx]->insert(extended_nodes_to_indices[neighbour_extended_node]);
            }
        }

        for (const auto &[key, neighbours] : *extended_stay_edges)
        {
            delete neighbours;
        }
        delete extended_stay_edges;

        // this->dg = new DirectedGraph(extended_indexed_stay_edges);
        this->dg = new DirectedGraph(extended_indexed_stay_edges, extended_nodes_to_indices.size()); 

        ///////////////////////

        // TODO: also consider node edges (edges with no h_change)
        // Map<Int_pair, Int_pair_SET *> *extended_edges = this->hg->get_extended_edges();

        // Map<int, Int_SET *> *indexed_edges = new Map<int, Int_SET *>();
        // Map<Int_pair*, int> extended_nodes_to_index;
        // for (const auto &[extended_node, neighbours] : *extended_edges)
        // {
        //     if (extended_nodes_to_index.find(extended_node) == extended_nodes_to_index.end())
        //     {
        //         extended_nodes_to_index[extended_node] = extended_nodes_to_index.size();
        //     }
        //     this->index_to_extended_node[extended_nodes_to_index[extended_node]] = extended_node;

        //     for (const auto &neighbour : *neighbours)
        //     {
        //         if (extended_nodes_to_index.find(neighbour) == extended_nodes_to_index.end())
        //         {
        //             extended_nodes_to_index[neighbour] = extended_nodes_to_index.size();
        //         }
        //         this->index_to_extended_node[extended_nodes_to_index[neighbour]] = neighbour;
        //         int node_idx = extended_nodes_to_index[extended_node];
        //         int neighbour_idx = extended_nodes_to_index[neighbour];

        //         if (indexed_edges->find(node_idx) == indexed_edges->end())
        //         {
        //             (*indexed_edges)[node_idx] = new Int_SET();
        //         }

        //         (*indexed_edges)[node_idx]->insert(neighbour_idx);
        //     }
        // }

        // for (const auto &[key, value] : *extended_edges)
        // {
        //     delete value;
        // }
        // delete extended_edges;

        // this->dg = new DirectedGraph(indexed_edges);
    }
    ~NoFlatExtendedCyclesCriterion()
    {
        delete this->dg;
    }

    SoundnessCheckResult check_soundness()
    {
        bool does_height_cycle_without_down_slopes_exist = this->dg->contains_cycle();

        if (!does_height_cycle_without_down_slopes_exist)
        {
            return SoundnessCheckResult::sound;
        }
        return SoundnessCheckResult::dontKnow;

        // Vec<Vec<Int_pair> *> *cycles = this->dg->get_elementary_cycles();

        // bool do_all_cycles_contain_down_slope = true;
        // for (const auto &cycle : *cycles)
        // {
        //     // for(const auto& [src_idx, sink_idx] : *cycle) {
        //     //     const auto [node_idx, height_idx] = this->index_to_extended_node.at(src_idx);
        //     //     printf("(%d,%d)", node_idx, height_idx);
        //     //     const auto [sink_node_idx, sink_height_idx] = this->index_to_extended_node.at(sink_idx);
        //     //     printf("(%d,%d)", sink_node_idx, sink_height_idx);
        //     // }

        //     // short circuit condition if found flat extended cycle
        //     if (do_all_cycles_contain_down_slope && !this->does_cycle_contain_down_slope(cycle))
        //     {
        //         do_all_cycles_contain_down_slope = false;
        //     };

        //     delete cycle;
        // }

        // delete cycles;
        // return do_all_cycles_contain_down_slope ? SoundnessCheckResult::sound : SoundnessCheckResult::dontKnow;
    }
};