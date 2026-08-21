#include "types.c"
#include "criterion.soundness.h"
#include "heighted_graph.h"

class DescendingUnicyclesCriterion : public SoundnessCriterion
{
private:
    Heighted_graph *hg;
    Vec<Vec<int> *> SCCs;

public:
    DescendingUnicyclesCriterion(Heighted_graph *hg);
    ~DescendingUnicyclesCriterion();

    SoundnessCheckResult check_soundness();
};