#pragma once

#include "types.c"
#include "criterion.soundness.hpp"
#include "heighted_graph.hpp"
#include <unistd.h>
#include <stdexcept>
#include <signal.h>

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
    ProcessBasedCriterion(Heighted_graph *hg)
    {
        this->hg = hg;
        int pipe_result = pipe(this->pipe_fd);
        if (pipe_result == -1)
        {
            printf("failed creating pipe %d", errno);
            throw std::runtime_error("failed creating pipe");
        }
    }
    ~ProcessBasedCriterion()
    {
        close(this->pipe_fd[0]);
        close(this->pipe_fd[1]);
        delete this->hg;
    }

    SoundnessCheckResult check_soundness()
    {
        Message result[1];
        this->criterion_pid = fork();
        if (this->criterion_pid == 0)
        {
            signal(SIGTERM, ProcessBasedCriterion::handle_signal);
            bool is_sound = this->soundness_check();
            Message msg = is_sound ? Message::sound : Message::unsound;
            write(this->pipe_fd[1], &msg, sizeof(Message));
            close(this->pipe_fd[1]);
            exit(1);
            return SoundnessCheckResult::dontKnow;
        }
        else
        {
            read(this->pipe_fd[0], result, sizeof(result));
            this->is_done = true;
            // close(this->pipe_fd[0]);
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

    static void handle_signal(int signum)
    {
        printf("got signal %d\n", signum);
        exit(1);
    }

    void halt()
    {
        // if (!(this->is_done) && this->criterion_pid > 0)
        if (this->criterion_pid > 0)
        {
            Message killed_msg = Message::killed;
            // TODO: this might be dangerous if halt is invoked after `check_soundness` is done
            int kill_result = kill(this->criterion_pid, SIGKILL);
            int criterion_process_status;
            waitpid(this->criterion_pid, &criterion_process_status, 0);
            if (!(this->is_done))
            {
                int write_result = write(this->pipe_fd[1], &killed_msg, sizeof(Message));
                // int close_result = close(this->pipe_fd[1]);
                if(write_result<0){
                    printf("error writing killed message\n");
                }
            }
        }
    }
};