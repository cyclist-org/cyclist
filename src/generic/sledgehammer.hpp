
#pragma once
#include "heighted_graph.hpp"
#include "criterion.soundness.hpp"
#include <future>

// Make sure this matches the criteria assigned in the ctor
const size_t CRITERIA_AMOUNT = 2;
const int FUTURE_WAIT_TIME_MS = 5;

class Sledgehammer
{
private:
    Heighted_graph *hg;
    std::shared_ptr<SoundnessCriterion> soundness_criteria[CRITERIA_AMOUNT];
    std::future<SoundnessCheckResult> soundness_results_futures[CRITERIA_AMOUNT];

    void start_all_criteria();
    bool wait_for_any_future();
    void halt_all_criteria();

public:
    Sledgehammer(Heighted_graph *hg, Heighted_graph::NODE_ORDER order, int opts);
    ~Sledgehammer();

    bool check_soundness();
};
