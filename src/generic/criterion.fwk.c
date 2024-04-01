#include "types.c"
#include "criterion.process_based.c"
#include "heighted_graph.hpp"

class FwkCriterion : public ProcessBasedCriterion
{
private:
    Heighted_graph *hg;
    int opts;

public:
    FwkCriterion(Heighted_graph *hg, int opts) : ProcessBasedCriterion(hg)
    {
        this->opts = opts;
        this->hg = hg;
    }

    bool soundness_check()
    {
        // auto start = std::chrono::system_clock::now();
        bool result = this->hg->fwk_check(this->opts, &(this->should_halt));
        // auto end = std::chrono::system_clock::now();
        // auto duration = end - start;
        // printf("fwk took %dus\n", duration);
        return result;
    }
};