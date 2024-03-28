#pragma once

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

public:
    virtual SoundnessCheckResult check_soundness() = 0;
    virtual void halt();
};
