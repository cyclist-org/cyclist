
#pragma once
#include "heighted_graph.hpp"
#include "criterion.soundness.hpp"
#include <future>

// Make sure this matches the criteria assigned in the ctor
// const size_t CRITERIA_AMOUNT = 3;
const int FUTURE_WAIT_TIME_MS = 3;

class Sledgehammer
{
private:
    Heighted_graph *hg;
    Vec<std::shared_ptr<SoundnessCriterion>> soundness_criteria;
    Vec<std::future<SoundnessCheckResult>> soundness_results_futures;

    void start_all_criteria();
    bool wait_for_any_future();
    void halt_all_criteria();

public:
    Sledgehammer(Heighted_graph *hg, Heighted_graph::NODE_ORDER order, int opts);
    ~Sledgehammer();

    bool check_soundness();
};
