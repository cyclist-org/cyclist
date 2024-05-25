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
#include <future>
#include <unistd.h>

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
    // std::string graph_str = this->hg->to_string();
    // this->hg->remove_down_edges_not_in_any_SCC();

    // flat cycles: complete unsound
    auto flat_cycles_start = std::chrono::system_clock::now();

    FlatCyclesCriterion flat_cycles_criterion(this->hg);
    auto resultFC = flat_cycles_criterion.check_soundness();

    // Heighted_graph* cloned_hg = Heighted_graph::clone(hg);
    // GeneralizedFlatCyclesCriterion generalized_flat_cycles_criterion(cloned_hg);
    // auto resultFC = generalized_flat_cycles_criterion.check_soundness();

    auto flat_cycles_end = std::chrono::system_clock::now();
    if (resultFC == SoundnessCheckResult::unsound)
    {
        printf("FC returned true negative after %lu us\n", flat_cycles_end-flat_cycles_start);
        fflush(stdout);
        // printf("unsound %s\n", graph_str.c_str());
        // fflush(stdout);
        return false;
    }

    
    // Sprenger Dam: sound incomplete
    // Heighted_graph* cloned_hg = Heighted_graph::clone(this->hg);
    // auto sprenger_dam_start = std::chrono::system_clock::now();
    // SprengerDamCriterion sprenger_dam_criterion(cloned_hg);
    // auto resultSD = sprenger_dam_criterion.check_soundness();
    // auto sprenger_dam_end = std::chrono::system_clock::now();
    // if (resultSD == SoundnessCheckResult::sound)
    // {
    //     printf("SD returned true negative after %lu us\n", sprenger_dam_end - sprenger_dam_start);
    //     fflush(stdout);
    //     // printf("unsound %s\n", graph_str.c_str());
    //     // fflush(stdout);
    //     return true;
    // }


    // no flat extended cycles: sound incomplete
    // auto no_flat_extended_cycles_start = std::chrono::system_clock::now();
    // NoFlatExtendedCyclesCriterion no_flat_extended_cycles_criterion(this->hg);
    // if (no_flat_extended_cycles_criterion.check_soundness() == SoundnessCheckResult::sound)
    // {
    //     auto no_flat_extended_cycles_end = std::chrono::system_clock::now();
    //     printf("NFEC returned true positive after %lu us\n", no_flat_extended_cycles_end - no_flat_extended_cycles_start);
    //     fflush(stdout);
    //     return true;
    // }

    auto OR_start = std::chrono::system_clock::now();
    auto result1 = this->hg->order_reduced_check(this->order, this->opts);
    auto OR_end = std::chrono::system_clock::now();
    // printf("OR returned after %lu us\n", OR_end - OR_start);
    printf("FC+OR returned after %lu us\n", OR_end - flat_cycles_start);
    fflush(stdout);

    // printf("%s %s\n", result1 ? "sound" : "unsound", graph_str.c_str());
    // fflush(stdout);

    return result1;

    this->start_all_criteria();
    bool result = this->wait_for_any_future();
    this->halt_all_criteria();

    return result;
    // return this->hg->order_reduced_check(this->order, this->opts);
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
