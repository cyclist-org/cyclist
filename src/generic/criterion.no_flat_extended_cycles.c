#pragma once

#include "types.c"
#include "criterion.soundness.hpp"
#include "heighted_graph.hpp"

class NoFlatExtendedCyclesCriterion : public SoundnessCriterion
{
private:
    Heighted_graph *hg;

public:
    NoFlatExtendedCyclesCriterion(Heighted_graph *hg)
    {
        this->hg = hg;
    }
    ~NoFlatExtendedCyclesCriterion()
    {
    }

    SoundnessCheckResult check_soundness()
    {
        // TODO
        return SoundnessCheckResult::dontKnow;
    }
};