#pragma once

#include "types.c"
#include "criterion.soundness.hpp"
#include "heighted_graph.hpp"
#include "directed_graph.c"

class FlatCyclesCriterion : public SoundnessCriterion
{
private:
    Heighted_graph *hg;
    DirectedGraph *dg;

public:
    FlatCyclesCriterion(Heighted_graph *hg)
    {
        this->hg = hg;
        Map<int, Int_SET *> *flat_edges = this->hg->get_flat_edges();
        this->dg = new DirectedGraph(flat_edges);
    }
    ~FlatCyclesCriterion()
    {
        // delete this->hg;
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