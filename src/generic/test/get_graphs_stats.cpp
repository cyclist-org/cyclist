#include "../types.c"
#include "../heighted_graph.hpp"
#include "../directed_graph.hpp"
#include "../sledgehammer.hpp"
#include <iostream>
#include <filesystem>
#include <regex>
#include <string>
#include "heighted_graph_parser.cpp"
#include "json.hpp"

using json = nlohmann::json;
namespace fs = std::filesystem;


int main(int argc, char **argv)
{
    if (argc > 1)
    {
        std::string path = argv[1];

        std::vector<fs::directory_entry> entries;
        for (const auto &entry : fs::directory_iterator(path))
        {
            entries.push_back(entry);
        }

        // Sort the entries by path
        std::sort(entries.begin(), entries.end(), [](const fs::directory_entry &a, const fs::directory_entry &b)
                  { return a.path().string() < b.path().string(); });

        for (const auto &entry : entries)
        {
            std::string path = entry.path();
            if (!entry.is_directory() && (path.find(".json") != std::string::npos))
            {
                bool is_sound = path.find("unsound") == std::string::npos;
                std::cout << path << "\n";

                // Get JSON data
                std::ifstream graph_data(path.c_str());
                json graph;
                graph_data >> graph;

                // Create heighted graph object
                Heighted_graph hg = Heighted_graph(graph["Node"].size());
                parse_from_json(graph, hg);
                DirectedGraph dg = DirectedGraph(hg.get_edges_adjacency_list(), hg.num_nodes());
                size_t num_of_heights = 0;
                size_t max_amount_of_heights = 0;
                for (size_t node_idx = 0; node_idx < hg.num_nodes(); node_idx++)
                {
                    size_t node_amount_of_heights = hg.get_HeightsOf()->at(node_idx)->size();
                    num_of_heights += node_amount_of_heights;
                    max_amount_of_heights = std::max(max_amount_of_heights, node_amount_of_heights);
                }

                printf("Graph file name: %s\n", entry.path().filename().c_str());
                // printf("Amount of nodes: %d\n", hg.num_nodes());
                // printf("Amount of edges: %d\n", hg.num_edges());
                printf("Amount of backedges: %d\n", dg.count_backedges());
                printf("Are there overlapping cycles: %s\n", dg.contains_overlapping_cycles() ? "yes" : "no");
                // printf("Amount of positions: %zu\n", num_of_heights);
                // printf("Proof width: %zu\n", max_amount_of_heights);
                fflush(stdout);
            }
        }
    }
}