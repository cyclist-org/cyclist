
#pragma once
#include "heighted_graph.hpp"
#include "criterion.soundness.hpp"
#include <future>

// Make sure this matches the criteria assigned in the ctor
// const size_t CRITERIA_AMOUNT = 3;
const int FUTURE_WAIT_TIME_MS = 3;

class Cyclone
{
private:
    Heighted_graph *hg;
    int opts;
    Heighted_graph::NODE_ORDER order;
    Vec<std::shared_ptr<SoundnessCriterion>> soundness_criteria;
    Vec<std::future<SoundnessCheckResult>> soundness_results_futures;

    void start_all_criteria();
    bool wait_for_any_future();
    void halt_all_criteria();


public:
    Cyclone(Heighted_graph *hg, Heighted_graph::NODE_ORDER order, int opts);
    ~Cyclone();

    bool check_soundness();
};
