#include "types.c"
#include "criterion.soundness.hpp"
#include "heighted_graph.hpp"
#include <unistd.h>
#include <stdexcept>
#include <signal.h>

class VlaCriterion : public SoundnessCriterion
{
private:
    Heighted_graph *hg;
    pid_t spot_pid;
    int pipe_fd[2];
    bool is_done = false;

    enum Message
    {
        sound = true,
        unsound = false,
        killed = 2
    };

public:
    VlaCriterion(Heighted_graph *hg)
    {
        this->hg = hg;
        int pipe_result = pipe(this->pipe_fd);
        if (pipe_result == -1)
        {
            throw std::runtime_error("failed creating pipe");
        }
    }
    ~VlaCriterion()
    {
        delete this->hg;
    }

    SoundnessCheckResult check_soundness()
    {
        Message result[1];
        this->spot_pid = fork();
        if (this->spot_pid == 0)
        {
            bool is_sound = this->hg->vla_automata_check();
            Message msg = is_sound ? Message::sound : Message::unsound;
            write(this->pipe_fd[1], &msg, sizeof(Message));
            close(this->pipe_fd[1]);
            return SoundnessCheckResult::dontKnow;
        }
        else
        {
            read(this->pipe_fd[0], result, sizeof(result));
            this->is_done = true;
            close(this->pipe_fd[0]);
            switch (*result)
            {
            case Message::sound:
                return SoundnessCheckResult::sound;
            case Message::unsound:
                return SoundnessCheckResult::unsound;
            case Message::killed:
                return SoundnessCheckResult::dontKnow;
            }
        }
    }

    void halt()
    {
        if (!(this->is_done))
        {
            Message killed_msg = Message::killed;
            // TODO: this might be dangerous if halt is invoked after `check_soundness` is done
            kill(this->spot_pid, SIGKILL);
            write(this->pipe_fd[1], &killed_msg, sizeof(Message));
        }
    }
};