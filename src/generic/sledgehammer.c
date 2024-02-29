#include "types.c"
#include "heighted_graph.hpp"
#include "sledgehammer.hpp"
#include "criterion.soundness.hpp"
#include "criterion.vla.c"
#include "criterion.sla.c"
#include "criterion.fwk.c"
#include "criterion.order_reduced.c"
#include <future>

Sledgehammer::Sledgehammer(Heighted_graph *hg, Heighted_graph::NODE_ORDER order, int opts)
{
    // this->soundness_criteria[0] = std::make_shared<VlaCriterion>(Heighted_graph::clone(hg));
    // this->soundness_criteria[1] = std::make_shared<SlaCriterion>(Heighted_graph::clone(hg));
    // this->soundness_criteria[2] = std::make_shared<FwkCriterion>(Heighted_graph::clone(hg), opts);
    // this->soundness_criteria[3] = std::make_shared<OrderReducedCriterion>(Heighted_graph::clone(hg), order, opts);
    this->soundness_criteria[0] = std::make_shared<FwkCriterion>(Heighted_graph::clone(hg), opts);
    this->soundness_criteria[1] = std::make_shared<OrderReducedCriterion>(Heighted_graph::clone(hg), order, opts);

}

Sledgehammer::~Sledgehammer()
{
}

bool Sledgehammer::check_soundness()
{
    this->start_all_criteria();
    bool result = this->wait_for_any_future();
    this->halt_all_criteria();
    return result;
}

void Sledgehammer::start_all_criteria()
{
    for (size_t i = 0; i < CRITERIA_AMOUNT; i++)
    {
        this->soundness_results_futures[i] = std::async(
            &SoundnessCriterion::check_soundness,
            this->soundness_criteria[i]);
    };
}

bool Sledgehammer::wait_for_any_future()
{
    bool any_ready = false;

    while (!any_ready)
    {
        for (size_t i = 0; i < CRITERIA_AMOUNT; i++)
        {
            std::future<SoundnessCheckResult> *curr_future = &(this->soundness_results_futures[i]);
            std::future_status curr_future_status =
                curr_future->wait_for(std::chrono::milliseconds(FUTURE_WAIT_TIME_MS));
            if (curr_future_status == std::future_status::ready)
            {
                auto curr_result = curr_future->get();
                if (curr_result != SoundnessCheckResult::dontKnow)
                {
                    printf("criterion number %zu done first\n", i);
                    return curr_result;
                }
            }
        }
    }
    return false;
}

void Sledgehammer::halt_all_criteria()
{
    for (size_t i = 0; i < CRITERIA_AMOUNT; i++)
    {
        this->soundness_criteria[i]->halt();
    }
}
