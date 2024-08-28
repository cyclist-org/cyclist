#include "types.c"
#include "heighted_graph.hpp"
#include "sledgehammer.hpp"
#include "criterion.soundness.hpp"
#include "criterion.vla.c"
#include "criterion.sla.c"
#include "criterion.fwk.c"
#include "criterion.order_reduced.c"
#include "criterion.flat_cycles.c"
#include "criterion.flat_cycles.generalized.c"
#include "criterion.sprenger_dam.c"
#include "criterion.no_flat_extended_cycles.c"
#include "criterion.descending_unicycles.c"
#include "criterion.trace_manifold.c"
#include <future>
#include <unistd.h>
#include <exception>

Sledgehammer::Sledgehammer(Heighted_graph *hg, Heighted_graph::NODE_ORDER order, int opts) : hg(hg), order(order), opts(opts)
{
    // this->soundness_criteria.push_back(std::make_shared<VlaCriterion>(hg));
    // this->soundness_criteria.push_back(std::make_shared<SlaCriterion>(hg));
    // this->soundness_criteria.push_back(std::make_shared<OrderReducedCriterion>(hg, order, opts));
    // this->soundness_criteria.push_back(std::make_shared<FlatCyclesCriterion>(hg));
}

Sledgehammer::~Sledgehammer()
{
}

bool Sledgehammer::check_soundness()
{
    // flat cycles: complete unsound
    // auto flat_cycles_start = std::chrono::system_clock::now();

    FlatCyclesCriterion flat_cycles_criterion(this->hg);
    auto resultFC = flat_cycles_criterion.check_soundness();

    // auto flat_cycles_end = std::chrono::system_clock::now();
    if (resultFC == SoundnessCheckResult::unsound)
    {
        return false;
    }

    DescendingUnicyclesCriterion descending_unicycles_criterion(hg);
    auto resultDU = descending_unicycles_criterion.check_soundness();
    if (resultDU == SoundnessCheckResult::sound)
    {
        return true;
    }
    if (resultDU == SoundnessCheckResult::unsound)
    {
        return false;
    }

    // TraceManifoldCriterion trace_manifold_criterion(hg);
    // auto resultTM = trace_manifold_criterion.check_soundness();
    // if (resultTM == SoundnessCheckResult::sound)
    // {
    //     return true;
    // }

    // auto OR_start = std::chrono::system_clock::now();
    auto resultOR = this->hg->order_reduced_check(this->order, this->opts);
    // auto OR_end = std::chrono::system_clock::now();
    // printf("OR returned after %lu us\n", OR_end - OR_start);
    // printf("FC+OR returned after %lu us\n", (OR_end - check_soundness_start).count());
    // fflush(stdout);

    // printf("%s %s\n", result1 ? "sound" : "unsound", graph_str.c_str());
    // fflush(stdout);

    return resultOR;

    // this->start_all_criteria();
    // bool result = this->wait_for_any_future();
    // this->halt_all_criteria();
    // return result;
}

void Sledgehammer::start_all_criteria()
{
    for (size_t i = 0; i < this->soundness_criteria.size(); i++)
    {
        this->soundness_results_futures.push_back(
            std::async(
                std::launch::async,
                &SoundnessCriterion::check_soundness,
                this->soundness_criteria[i]));
    };
}

bool Sledgehammer::wait_for_any_future()
{
    bool any_ready = false;

    Int_SET criteria_that_are_done;

    while (criteria_that_are_done.size() < this->soundness_criteria.size())
    {
        for (size_t i = 0; i < this->soundness_criteria.size(); i++)
        {
            if (criteria_that_are_done.find(i) != criteria_that_are_done.end())
            {
                continue;
            }
            std::future<SoundnessCheckResult> *curr_future = &(this->soundness_results_futures[i]);
            std::future_status curr_future_status =
                curr_future->wait_for(std::chrono::milliseconds(FUTURE_WAIT_TIME_MS));
            if (curr_future_status == std::future_status::ready)
            {
                criteria_that_are_done.insert(i);
                auto curr_result = curr_future->get();
                if (curr_result != SoundnessCheckResult::dontKnow)
                {
                    // printf("criterion %s done first\n", this->soundness_criteria[i]->get_criterion_name().c_str());
                    return curr_result == SoundnessCheckResult::sound;
                }
            }
        }
    }
    throw std::runtime_error("no criterion returned an answer");
}

void Sledgehammer::halt_all_criteria()
{
    for (size_t i = 0; i < this->soundness_criteria.size(); i++)
    {
        this->soundness_criteria[i]->halt();
    }
}
