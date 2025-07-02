#include "criterion.soundness.hpp"
#include "heighted_graph.hpp"

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