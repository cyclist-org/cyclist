#include "types.c"
#include "criterion.soundness.hpp"
#include "heighted_graph.hpp"
#include "directed_graph.hpp"

/*
    Checks if the heighted graph (without the downward edges that are not in any SCC)
    has a flat cycle
*/
class GeneralizedFlatCyclesCriterion : public SoundnessCriterion
{
private:
    Heighted_graph *hg;
    DirectedGraph *dg;

public:
    GeneralizedFlatCyclesCriterion(Heighted_graph *hg)
    {
        this->hg = hg;
        this->hg->remove_down_edges_not_in_any_SCC();

        // auto start = std::chrono::system_clock::now();
        Map<int, Int_SET *> *flat_edges = this->hg->get_flat_edges();
        // auto end = std::chrono::system_clock::now();
        // printf("getting flat edges took %d us\n", end-start);

        this->dg = new DirectedGraph(flat_edges, hg->num_nodes());
    }
    ~GeneralizedFlatCyclesCriterion()
    {
        delete this->hg;
        delete this->dg;
    }

    SoundnessCheckResult check_soundness()
    {
        // auto start = std::chrono::system_clock::now();
        bool does_flat_cycle_exist = this->dg->contains_cycle();
        // auto end = std::chrono::system_clock::now();
        // printf("checking if cycle exists took %d us\n", end-start);

        if (does_flat_cycle_exist)
        {
            return SoundnessCheckResult::unsound;
        }
        return SoundnessCheckResult::dontKnow;
    }
};