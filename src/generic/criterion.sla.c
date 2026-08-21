#include "types.c"
#include "criterion.soundness.h"
#include "criterion.process_based.h"
#include "heighted_graph.h"

class SlaCriterion : public ProcessBasedCriterion
{

public:
    SlaCriterion(Heighted_graph *hg) : ProcessBasedCriterion(hg)
    {
        this->criterion_name = "SLA";
    }

    bool soundness_check()
    {
        // auto start =  std::chrono::system_clock::now();
        bool result = this->hg->sla_automata_check();
        // auto end = std::chrono::system_clock::now();
        // auto duration = end - start;
        // printf("sla took %dus\n", duration);
        return result;
    }
};