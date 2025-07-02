#include "types.c"
#include "criterion.soundness.hpp"
#include "heighted_graph.hpp"
#include "graph.hpp"

class SprengerDamCriterion : public SoundnessCriterion
{
private:
    Heighted_graph *hg;
    Graph *graph;

public:
    SprengerDamCriterion(Heighted_graph *hg)
    {
        this->hg = hg;
        this->graph = new Graph(this->hg->get_edges(), this->hg->get_HeightsOf(), this->hg->get_h_change(), this->hg->get_max_nodes());
    }
    ~SprengerDamCriterion()
    {
        delete this->hg;
        delete this->graph;
    }

    SoundnessCheckResult check_soundness()
    {
        bool does_flat_cycle_exist = this->graph->check_SD(SD_decrease_type::STD);

        if (does_flat_cycle_exist)
        {
            return SoundnessCheckResult::sound;
        }
        return SoundnessCheckResult::dontKnow;
    }
};