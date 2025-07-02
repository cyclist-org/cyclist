#include "../types.c"
#include "../heighted_graph.hpp"
#include "../directed_graph.hpp"
#include "../cyclone.hpp"
#include <iostream>
#include <filesystem>
#include <regex>
#include <string>
#include "heighted_graph_parser.cpp"
#include "json.hpp"
#include "../criterion.descending_unicycles.c"
#include "../criterion.trace_manifold.hpp"
#include "../criterion.flat_cycles.generalized.c"

using json = nlohmann::json;
namespace fs = std::filesystem;

int main(int argc, char **argv)
{
    if (argc > 1)
    {
        std::string path = argv[1];
        bool only_minimised = false; // TODO: make this a flag

        std::vector<fs::directory_entry> entries;
        for (const auto &entry : fs::directory_iterator(path))
        {
            if (only_minimised)
            {
                if (entry.path().string().find("minimised") != std::string::npos)
                {
                    entries.push_back(entry);
                }
            }
            else
            {
                entries.push_back(entry);
            }
        }

        // Sort the entries by path
        std::sort(entries.begin(), entries.end(), [](const fs::directory_entry &a, const fs::directory_entry &b)
                  { return a.path().string() < b.path().string(); });

        int amount_of_false_positives = 0;
        int amount_of_true_positives = 0;
        for (const auto &entry : entries)
        {
            std::string path = entry.path();
            if (!entry.is_directory() && (path.find(".json") != std::string::npos)
                // && (path.find("minimised") != std::string::npos)
            )
            {
                std::cout << path << "\n";

                // Get JSON data
                std::ifstream graph_data(path.c_str());
                json graph;
                graph_data >> graph;

                // Create heighted graph object
                Heighted_graph hg = Heighted_graph(graph["Node"].size());
                parse_from_json(graph, hg);

                int flags = Heighted_graph::FAIL_FAST | Heighted_graph::USE_MINIMALITY | Heighted_graph::USE_TRANSITIVE_LOOPING;

                bool is_sound = hg.order_reduced_check(Heighted_graph::NODE_ORDER::GIVEN_ORDER, flags);
                TraceManifoldCriterion tm(&hg);
                auto result = tm.check_soundness();
                if ((result == SoundnessCheckResult::sound) && (is_sound == false))
                {
                    amount_of_false_positives++;
                    printf("TM false positive: %s\n", path.c_str());
                }
                if ((result == SoundnessCheckResult::sound) && (is_sound == true))
                {
                    amount_of_true_positives++;
                }
            }
        }

        printf("amount of TM false positives: %d\n", amount_of_false_positives);
        printf("amount of TM true positives: %d\n", amount_of_true_positives);
    }
}