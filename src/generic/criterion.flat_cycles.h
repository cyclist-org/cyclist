#include "criterion.soundness.h"
#include "heighted_graph.h"

class FlatCyclesCriterion : public SoundnessCriterion
{
private:
    Heighted_graph *hg;
    // DirectedGraph *dg;

public:
    FlatCyclesCriterion(Heighted_graph *hg);
    ~FlatCyclesCriterion();

    SoundnessCheckResult check_soundness();
};