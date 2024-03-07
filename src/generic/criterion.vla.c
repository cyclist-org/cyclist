#include "types.c"
#include "criterion.soundness.hpp"
#include "criterion.automata_based.c"
#include "heighted_graph.hpp"

class VlaCriterion : public AutomataBasedCriterion
{

public:
    VlaCriterion(Heighted_graph *hg) : AutomataBasedCriterion(hg)
    {
    }

    bool automata_check()
    {
        return this->hg->vla_automata_check();
    }
};