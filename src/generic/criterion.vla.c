#include "types.c"
#include "criterion.soundness.hpp"
#include "heighted_graph.hpp"

class VlaCriterion : public SoundnessCriterion
{
private:
    Heighted_graph* hg;

public:
    VlaCriterion(Heighted_graph* hg)
    {
        this->hg = hg;
    }
    ~VlaCriterion()
    {
        delete this->hg;
    }

    SoundnessCheckResult check_soundness()
    {
        bool result = this->hg->vla_automata_check();
        return (result ? SoundnessCheckResult::sound : SoundnessCheckResult::unsound);
    }
};