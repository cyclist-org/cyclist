#include "types.c"
#include "criterion.soundness.hpp"
#include "criterion.process_based.c"
#include "heighted_graph.hpp"

class VlaCriterion : public ProcessBasedCriterion
{

public:
    VlaCriterion(Heighted_graph *hg) : ProcessBasedCriterion(hg)
    {
        this->criterion_name = "VLA";
    }

    bool soundness_check()
    {
        // auto start = std::chrono::system_clock::now();
        bool result = this->hg->vla_automata_check();
        // auto end = std::chrono::system_clock::now();
        // auto duration = end - start;
        // printf("vla took %dus\n", duration);
        return result;
    }
};