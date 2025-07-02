#include "criterion.flat_cycles.hpp"

FlatCyclesCriterion::FlatCyclesCriterion(Heighted_graph *hg)
{
    this->hg = hg;
    // Map<int, Int_SET *> *flat_edges = this->hg->get_flat_edges();
    // this->dg = new DirectedGraph(flat_edges, hg->num_nodes());
}
FlatCyclesCriterion::~FlatCyclesCriterion()
{
    // delete this->hg;
    // delete this->dg;
}

SoundnessCheckResult FlatCyclesCriterion::check_soundness()
{
    // bool does_flat_cycle_exist = this->dg->contains_cycle();
    bool does_flat_cycle_exist = this->hg->has_flat_cycle();

    if (does_flat_cycle_exist)
    {
        return SoundnessCheckResult::unsound;
    }
    return SoundnessCheckResult::dontKnow;
}