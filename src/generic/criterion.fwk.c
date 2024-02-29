#include "types.c"
#include "criterion.soundness.hpp"
#include "heighted_graph.hpp"

class FwkCriterion : public SoundnessCriterion
{
private:
    Heighted_graph* hg;
    int opts;

public:
    FwkCriterion(Heighted_graph* hg, int opts)
    {
        this->opts = opts;
        this->hg = hg;
    }
    ~FwkCriterion()
    {
        delete this->hg;
    }

    SoundnessCheckResult check_soundness()
    {
        bool result = this->hg->fwk_check(this->opts, &(this->should_halt));
        return (result ? SoundnessCheckResult::sound : SoundnessCheckResult::unsound);
    }
};