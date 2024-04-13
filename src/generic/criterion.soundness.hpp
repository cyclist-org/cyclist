#pragma once
#include <string>

enum SoundnessCheckResult
{
    dontKnow,
    sound,
    unsound,
};

class SoundnessCriterion
{
protected:
    bool should_halt = false;
    std::string criterion_name;

public:
    virtual SoundnessCheckResult check_soundness() = 0;
    virtual void halt();
    std::string get_criterion_name();
};
