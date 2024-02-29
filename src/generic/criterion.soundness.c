#include "criterion.soundness.hpp"

void SoundnessCriterion::halt()
{
    this->should_halt = true;
}
