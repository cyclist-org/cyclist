#include "types.c"
#include "criterion.soundness.hpp"
#include "heighted_graph.hpp"

class OrderReducedCriterion : public SoundnessCriterion
{
private:
    Heighted_graph *hg;
    Heighted_graph::NODE_ORDER order;
    int opts;

public:
    OrderReducedCriterion(Heighted_graph *hg, Heighted_graph::NODE_ORDER order, int opts)
    {
        this->opts = opts;
        this->hg = hg;
    }
    ~OrderReducedCriterion()
    {
        delete this->hg;
    }

    SoundnessCheckResult check_soundness()
    {
        bool result = this->hg->order_reduced_check(this->order, this->opts, &(this->should_halt));
        return (result ? SoundnessCheckResult::sound : SoundnessCheckResult::unsound);
    }
};