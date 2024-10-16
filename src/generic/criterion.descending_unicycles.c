#include "criterion.descending_unicycles.hpp"

DescendingUnicyclesCriterion::DescendingUnicyclesCriterion(Heighted_graph *hg)
{
    this->hg = hg;
}

DescendingUnicyclesCriterion::~DescendingUnicyclesCriterion()
{
    // delete this->hg;
    for (const auto &SCC : this->SCCs)
    {
        delete SCC;
    }
}

SoundnessCheckResult DescendingUnicyclesCriterion::check_soundness()
{
    bool has_overlapping_cycles = this->hg->calculate_SCCs_and_check_if_has_overlapping_cycles(this->SCCs);
    if (has_overlapping_cycles)
    {
        return SoundnessCheckResult::dontKnow;
    }

    // for (const auto &SCC : this->SCCs) {
    //     for (const auto &n : *SCC) {
    //         printf("%d,", n);
    //     }
    //     printf("\n");
    // }


    for (const auto &SCC : this->SCCs)
    {
        if (SCC->size() == 1)
        {
            if (!this->hg->has_self_edge(SCC->at(0))){
                continue;
            }
        }
        if (!this->hg->does_node_SCC_contain_a_down_extended_SCC(SCC))
        {
            return SoundnessCheckResult::unsound;
        }
    }
    return SoundnessCheckResult::sound;
}