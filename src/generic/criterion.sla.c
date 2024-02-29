#include "types.c"
#include "criterion.soundness.hpp"
#include "heighted_graph.hpp"

class SlaCriterion : public SoundnessCriterion
{
private:
    Heighted_graph* hg;

public:
    SlaCriterion(Heighted_graph* hg)
    {
        this->hg = hg;
    }
    ~SlaCriterion()
    {
        delete this->hg;
    }

    SoundnessCheckResult check_soundness()
    {
        bool result = this->hg->sla_automata_check();
        return (result ? SoundnessCheckResult::sound : SoundnessCheckResult::unsound);
    }
};