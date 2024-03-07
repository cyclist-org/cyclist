#include "types.c"
#include "criterion.soundness.hpp"
#include "criterion.automata_based.c"
#include "heighted_graph.hpp"

class SlaCriterion : public AutomataBasedCriterion
{

public:
    SlaCriterion(Heighted_graph *hg) : AutomataBasedCriterion(hg)
    {
    }

    bool automata_check()
    {
        return this->hg->sla_automata_check();
    }
};