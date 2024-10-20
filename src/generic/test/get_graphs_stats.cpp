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
#include "../criterion.descending_unicycles.hpp"
#include "../criterion.trace_manifold.hpp"

using json = nlohmann::json;
namespace fs = std::filesystem;

bool has_non_oneshot_cycle(Heighted_graph &hg, DirectedGraph &dg)
{
    // Graph graph(hg.get_edges(), hg.get_HeightsOf(), hg.get_h_change(), hg.get_max_nodes());
    // graph.get_ECycles();
    // auto elementary_cycles = graph.ECycles;
    auto elementary_cycles = dg.get_elementary_cycles();
    printf("Amount of elementary cycles: %zu\n", elementary_cycles->size());

    for (const auto &cycle : *elementary_cycles)
    {
        if (!hg.is_cycle_oneshot(*cycle))
        {
            return true;
        }
    }
    return false;
}

bool are_all_relations_partial_functions(Heighted_graph &hg)
{
    auto h_change = hg.get_h_change();
    for (int i = 0; i < hg.num_nodes(); i++)
    {
        for (int j = 0; j < hg.num_nodes(); j++)
        {
            auto rel = h_change[i][j];
            if (rel != NULL)
            {
                if (!rel->is_partial_function())
                {
                    return false;
                }
            }
        }
    }
    return true;
}

void measure_flat_cycles_runtime(Heighted_graph &hg)
{
    auto start = std::chrono::system_clock::now();
    bool result = hg.has_flat_cycle();
    auto end = std::chrono::system_clock::now();
    std::string answer = result ? "no" : "don't know";
    std::cout << "flat cycles answer: " << answer << " took " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count() << "\n";
}

void measure_descending_unicycles_runtime(Heighted_graph &hg)
{
    DescendingUnicyclesCriterion criterion(&hg);
    auto start = std::chrono::system_clock::now();
    SoundnessCheckResult result = criterion.check_soundness();
    auto end = std::chrono::system_clock::now();
    std::string answer = result == SoundnessCheckResult::sound ? "yes" : result == SoundnessCheckResult::unsound ? "no"
                                                                                                                 : "don't know";
    std::cout << "descending unicycles answer: " << answer << " took " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count() << "\n";
}
void measure_trace_manifold_runtime(Heighted_graph &hg)
{
    TraceManifoldCriterion criterion(&hg);
    auto start = std::chrono::system_clock::now();
    SoundnessCheckResult result = criterion.check_soundness();
    std::string answer = result == SoundnessCheckResult::sound ? "yes" : result == SoundnessCheckResult::unsound ? "no"
                                                                                                                 : "don't know";
    auto end = std::chrono::system_clock::now();
    std::cout << "trace manifold answer: " << answer << " took " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count() << "\n";
}
void measure_order_reduced_runtime(Heighted_graph &hg)
{
    int flags = Heighted_graph::FAIL_FAST | Heighted_graph::USE_MINIMALITY | Heighted_graph::USE_SCC_CHECK;
    auto start = std::chrono::system_clock::now();
    bool result = hg.order_reduced_check(Heighted_graph::NODE_ORDER::GIVEN_ORDER, flags);
    auto end = std::chrono::system_clock::now();
    std::string answer = result ? "yes" : "no";
    std::cout << "order reduced answer: " << answer << " took " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count() << "\n";
}

int main(int argc, char **argv)
{
    if (argc > 1)
    {
        std::string path = argv[1];
        bool only_minimised = false; //TODO: make this a flag

        std::vector<fs::directory_entry> entries;
        for (const auto &entry : fs::directory_iterator(path))
        {
            if(only_minimised){
                if(entry.path().string().find("minimised")!= std::string::npos) {
                    entries.push_back(entry);
                }
            } else {
                entries.push_back(entry);
            }
        }

        // Sort the entries by path
        std::sort(entries.begin(), entries.end(), [](const fs::directory_entry &a, const fs::directory_entry &b)
                  { return a.path().string() < b.path().string(); });

        for (const auto &entry : entries)
        {
            std::string path = entry.path();
            if (!entry.is_directory() && (path.find(".json") != std::string::npos)
                // && (path.find("minimised") != std::string::npos)
            )
            {
                // bool is_sound = path.find("unsound") == std::string::npos;
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
                std::vector<std::vector<int> *> SCCs;

                printf("Graph file name: %s\n", entry.path().filename().c_str());

                int flags = Heighted_graph::FAIL_FAST | Heighted_graph::USE_MINIMALITY | Heighted_graph::USE_SCC_CHECK;

                // is_sound = hg.order_reduced_check(Heighted_graph::NODE_ORDER::GIVEN_ORDER, flags);
                // printf("Is sound? %s\n", result == SoundnessCheckResult::sound ? "yes" : result == SoundnessCheckResult::unsound ? "no": "don't know");
                // printf("Is sound? %s\n", is_sound ? "yes" : "no");
                // printf("Has flat cycle? %s\n", hg.has_flat_cycle() ? "yes" : "no");

                // printf("Amount of nodes: %d\n", hg.num_nodes());
                printf("Amount of edges: %d\n", hg.num_edges());
                printf("Amount of backedges: %d\n", dg.count_backedges());
                // printf("Are there overlapping cycles: %s\n", hg.calculate_SCCs_and_check_if_has_overlapping_cycles(SCCs) ? "yes" : "no");
                auto structural_connectivity_relation = hg.get_structural_connectivity_relation();
                printf("Is in cycle normal form? %s\n", structural_connectivity_relation.is_cyclic_normal_form ? "yes" : "no");

                auto trace_manifold_graph = hg.get_trace_manifold_graph(structural_connectivity_relation);
                int amount_of_nodes_in_trace_manifold_graph = 0;
                for (auto traces_of_cycle : *trace_manifold_graph.trace_node_per_cycle)
                {
                    amount_of_nodes_in_trace_manifold_graph += traces_of_cycle->size();
                }
                printf("Amount of nodes in trace manifold graph: %d\n", amount_of_nodes_in_trace_manifold_graph);
                printf("Amount of edges in trace manifold graph: %d\n", trace_manifold_graph.edges->size());

                int amount_of_positions_in_all_cycles = 0;
                const auto &heights_of = hg.get_HeightsOf();
                bool has_overlapping_cycles = hg.calculate_SCCs_and_check_if_has_overlapping_cycles(SCCs);

                int amount_of_SCCs = 0;
                for (auto &SCC : SCCs)
                {
                    if (SCC->size() == 1)
                    {
                        if (hg.has_self_edge(SCC->at(0)))
                        {
                            amount_of_SCCs++;
                        }
                    }
                    else
                    {
                        amount_of_SCCs++;
                    }
                    for (auto &node : *SCC)
                    {
                        amount_of_positions_in_all_cycles += heights_of->at(node)->size();
                    }
                }
                printf("Amount of positions in all cycles: %d\n", amount_of_positions_in_all_cycles);
                printf("Amount of SCCs: %d\n", amount_of_SCCs);

                printf("Has overlapping cycles? %s\n", dg.contains_overlapping_cycles() ? "yes" : "no");
                // printf("Has non-oneshot cycle? %s\n", has_non_oneshot_cycle(hg, dg) ? "yes" : "no");
                // printf("Amount of positions: %zu\n", num_of_heights);
                printf("Proof width: %zu\n", max_amount_of_heights);

                printf("are all relations partial functions? %s\n", are_all_relations_partial_functions(hg) ? "yes" : "no");

                Heighted_graph::StructuralConnectivityRelation relation = hg.get_structural_connectivity_relation();
                printf("size of structural connectivity relation: %d\n", relation.relation->size());
                // printf("\n");

                // printf("Is cyclic normal form? %s\n", relation.is_cyclic_normal_form ? "yes" : "no");

                // for (const auto &[companion, cycle_idx] : *(relation.companions))
                // {
                //     printf("companion: %d, cycle index: %d\n", companion, cycle_idx);
                //     printf("cycle nodes: ");
                //     for(const auto& cycle_node: *(relation.cycles->at(cycle_idx))) {
                //         printf("%d ", cycle_node);
                //     }
                //     printf("\n");
                // }
                // printf("\n");
                // for(const auto& edge : *(relation.relation)) {
                //     printf("%d<%d\n", edge.first, edge.second);
                // }

                measure_flat_cycles_runtime(hg);
                // measure_generalized_flat_cycles_runtime(hg);
                measure_descending_unicycles_runtime(hg);
                measure_trace_manifold_runtime(hg);
                measure_order_reduced_runtime(hg);

                printf("\n");
                fflush(stdout);
            }
        }
    }
}
