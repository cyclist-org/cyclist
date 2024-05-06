#pragma once

#include "criterion.soundness.hpp"
#include "heighted_graph.hpp"

class ProcessBasedCriterion : public SoundnessCriterion
{
private:
    pid_t criterion_pid;
    int pipe_fd[2];
    bool is_done = false;

    enum Message
    {
        sound = true,
        unsound = false,
        killed = 2
    };

protected:
    Heighted_graph *hg;
    virtual bool soundness_check() = 0;

public:
    ProcessBasedCriterion(Heighted_graph *hg);
    ~ProcessBasedCriterion();

    SoundnessCheckResult check_soundness();
    void halt();
};