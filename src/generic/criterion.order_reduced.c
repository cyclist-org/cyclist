#include "types.c"
#include "criterion.process_based.c"
#include "heighted_graph.hpp"

class OrderReducedCriterion : public ProcessBasedCriterion
{
private:
    Heighted_graph *hg;
    Heighted_graph::NODE_ORDER order;
    int opts;

public:
    OrderReducedCriterion(Heighted_graph *hg, Heighted_graph::NODE_ORDER order, int opts) : ProcessBasedCriterion(hg)
    {
        this->opts = opts;
        this->hg = hg;
    }

    bool soundness_check()
    {
        // auto start = std::chrono::system_clock::now();
        bool result = this->hg->order_reduced_check(this->order, this->opts, &(this->should_halt));
        // auto end = std::chrono::system_clock::now();
        // auto duration = end - start;
        // printf("order reduced took %dus\n", duration);

        return result;
    }
};