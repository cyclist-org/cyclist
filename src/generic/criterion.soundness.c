#include "criterion.soundness.h"

void SoundnessCriterion::halt()
{
    this->should_halt = true;
}

std::string SoundnessCriterion::get_criterion_name()
{
    return this->criterion_name;
}