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
         auto start = std::chrono::system_clock::now();
        bool result = this->hg->fwk_check(this->opts, &(this->should_halt));
        auto end = std::chrono::system_clock::now();
        auto duration = end - start;
        printf("fwk took %dus\n", duration);
        return (result ? SoundnessCheckResult::sound : SoundnessCheckResult::unsound);
    }
};