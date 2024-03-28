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
        auto start = std::chrono::system_clock::now();
        bool result = this->hg->vla_automata_check();
        auto end = std::chrono::system_clock::now();
        auto duration = end - start;
        printf("vla took %dus\n", duration);
        return result;
    }
};