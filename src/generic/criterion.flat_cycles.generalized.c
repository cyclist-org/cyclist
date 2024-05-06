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
        this->hg = Heighted_graph::clone(hg);
        this->hg->remove_down_edges_not_in_any_SCC();
        Map<int, Int_SET *> *flat_edges = this->hg->get_flat_edges();
        this->dg = new DirectedGraph(flat_edges);
    }
    ~GeneralizedFlatCyclesCriterion()
    {
        delete this->hg;
        delete this->dg;
    }

    SoundnessCheckResult check_soundness()
    {
        bool does_flat_cycle_exist = this->dg->contains_cycle();

        if (does_flat_cycle_exist)
        {
            return SoundnessCheckResult::unsound;
        }
        return SoundnessCheckResult::dontKnow;
    }
};