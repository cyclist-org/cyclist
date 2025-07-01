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

template <typename... Args>
std::string string_format(const std::string &format, Args... args)
{
    int size_s = std::snprintf(nullptr, 0, format.c_str(), args...) + 1; // Extra space for '\0'
    if (size_s <= 0)
    {
        throw std::runtime_error("Error during formatting.");
    }
    auto size = static_cast<size_t>(size_s);
    std::unique_ptr<char[]> buf(new char[size]);
    std::snprintf(buf.get(), size, format.c_str(), args...);
    return std::string(buf.get(), buf.get() + size - 1); // We don't want the '\0' inside
}

inline std::string bool_to_string(bool b)
{
    return b ? "yes" : "no";
}

class OutputRow
{
public:
    std::string test_suite;
    std::string graph_name;
    int width;
    int buds;
    int edges;
    int nodes;
    int amount_of_backedges;
    int amounts_of_trace_manifold_graph_edges;
    int amounts_of_trace_manifold_graph_nodes;
    int size_of_structural_connectivity_relation;
    int amounts_of_positions_in_all_cycles;
    int amount_of_SCCs;
    bool has_overlapping_cycles;
    bool is_in_cycle_normal_form;
    int FC_duration_us;
    std::string FC_answer;
    int DU_duration_us;
    std::string DU_answer;
    int TM_duration_us;
    std::string TM_answer;
    int OR_duration_us;
    std::string OR_answer;

    static std::string get_header_csv()
    {
        return "test suite,graph name,width,buds,edges,nodes,amount of backedges,amount of trace manifold graph edges,amount of trace manifold graph nodes,size of structural connectivity relation,amount of positions in all cycles,amount of SCCs,has overlapping cycles,is in cycle normal form,FC duration microseconds,FC answer,DU duration microseconds,DU answer,TM duration microseconds,TM answer,OR duration microseconds,OR answer\n";
    }

    std::string to_csv_string()
    {
        return string_format(
            "%s,%s,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%s,%s,%d,%s,%d,%s,%d,%s,%d,%s\n",
            this->test_suite.c_str(), this->graph_name.c_str(), this->width, this->buds, this->edges, this->nodes, this->amount_of_backedges, this->amounts_of_trace_manifold_graph_edges, this->amounts_of_trace_manifold_graph_nodes, this->size_of_structural_connectivity_relation, this->amounts_of_positions_in_all_cycles, this->amount_of_SCCs, bool_to_string(this->has_overlapping_cycles).c_str(), bool_to_string(this->is_in_cycle_normal_form).c_str(), this->FC_duration_us, this->FC_answer.c_str(), this->DU_duration_us, this->DU_answer.c_str(), this->TM_duration_us, this->TM_answer.c_str(), this->OR_duration_us, this->OR_answer.c_str());
    }
};

Pair<int, std::string> measure_flat_cycles_runtime(Heighted_graph &hg, int runs_repetitions)
{
    assert(runs_repetitions > 0);
    std::string answer;
    int duration_us = 0;
    for (int i = 0; i < runs_repetitions; i++)
    {
        auto start = std::chrono::system_clock::now();
        bool result = hg.has_flat_cycle();
        auto end = std::chrono::system_clock::now();
        answer = result ? "no" : "don't know";
        duration_us += std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();
    }
    // std::cout << "flat cycles answer: " << answer << " took " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count() << "\n";
    return Pair(duration_us/runs_repetitions, answer);
}

Pair<int, std::string> measure_descending_unicycles_runtime(Heighted_graph &hg, int runs_repetitions)
{
    assert(runs_repetitions > 0);
    std::string answer;
    int duration_us = 0;
    for (int i = 0; i < runs_repetitions; i++)
    {
        DescendingUnicyclesCriterion criterion(&hg);
        auto start = std::chrono::system_clock::now();
        SoundnessCheckResult result = criterion.check_soundness();
        auto end = std::chrono::system_clock::now();
        answer = result == SoundnessCheckResult::sound ? "yes" : result == SoundnessCheckResult::unsound ? "no"
                                                                                                         : "don't know";
        duration_us += std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();
    }
    // std::cout << "descending unicycles answer: " << answer << " took " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count() << "\n";
    return Pair(duration_us/runs_repetitions, answer);
}
Pair<int, std::string> measure_trace_manifold_runtime(Heighted_graph &hg, int runs_repetitions)
{
    assert(runs_repetitions > 0);
    std::string answer;
    int duration_us = 0;
    for (int i = 0; i < runs_repetitions; i++)
    {
        TraceManifoldCriterion criterion(&hg);
        auto start = std::chrono::system_clock::now();
        SoundnessCheckResult result = criterion.check_soundness();
        answer = result == SoundnessCheckResult::sound ? "yes" : result == SoundnessCheckResult::unsound ? "no"
                                                                                                                     : "don't know";
        auto end = std::chrono::system_clock::now();
        duration_us += std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();
    }
    // std::cout << "trace manifold answer: " << answer << " took " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count() << "\n";
    return Pair(duration_us/runs_repetitions, answer);
}
Pair<int, std::string> measure_order_reduced_runtime(Heighted_graph &hg, int runs_repetitions)
{
    assert(runs_repetitions > 0);
    std::string answer;
    int duration_us = 0;
    for (int i = 0; i < runs_repetitions; i++)
    {
    int flags = Heighted_graph::FAIL_FAST | Heighted_graph::USE_MINIMALITY | Heighted_graph::USE_TRANSITIVE_LOOPING;
    auto start = std::chrono::system_clock::now();
    bool result = hg.order_reduced_check(Heighted_graph::NODE_ORDER::GIVEN_ORDER, flags);
    auto end = std::chrono::system_clock::now();
    answer = result ? "yes" : "no";
    duration_us += std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();
    }
    // std::cout << "order reduced answer: " << answer << " took " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count() << "\n";
    return Pair(duration_us/runs_repetitions, answer);
}

int main(int argc, char **argv)
{
    bool only_minimised = false; // TODO: make this a flag
    if (argc > 1)
    {
        int runs_repetitions = argc > 2 ? std::max(atoi(argv[2]), 1) : 1;
        // std::string path = argv[1];
        std::filesystem::path path = argv[1];
        Vec<std::filesystem::path> test_suites = {"fo", "sl"};

        std::cout << OutputRow::get_header_csv();

        for (const auto test_suite : test_suites)
        {
            std::filesystem::path test_suite_path = path / test_suite;

            std::vector<fs::directory_entry> entries;
            for (const auto &entry : fs::directory_iterator(test_suite_path))
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

            for (const auto &entry : entries)
            {
                std::string path = entry.path();
                if (!entry.is_directory() && (path.find(".json") != std::string::npos))
                {
                    // std::cout << path << "\n";

                    if (std::filesystem::file_size(path)==0){
                        continue;
                    }
                    // Get JSON data
                    std::ifstream graph_data(path.c_str());
                    json graph;
                    graph_data >> graph;

                    // Create heighted graph object
                    Heighted_graph hg = Heighted_graph(graph["Node"].size());
                    parse_from_json(graph, hg, false);

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

                    // printf("Graph file name: %s\n", entry.path().filename().c_str());

                    int flags = Heighted_graph::FAIL_FAST | Heighted_graph::USE_MINIMALITY | Heighted_graph::USE_TRANSITIVE_LOOPING;

                    // printf("Amount of nodes: %d\n", hg.num_nodes());
                    // printf("Amount of edges: %d\n", hg.num_edges());
                    // printf("Amount of backedges: %d\n", dg.count_backedges());
                    int nodes = hg.num_nodes();
                    int edges = hg.num_edges();
                    int buds = 0;
                    for (auto &bud : graph["Bud"])
                    {
                        buds++;
                    }
                    int amount_of_backedges = dg.count_backedges();

                    auto structural_connectivity_relation = hg.get_structural_connectivity_relation();
                    // printf("Is in cycle normal form? %s\n", structural_connectivity_relation.is_cyclic_normal_form ? "yes" : "no");
                    bool is_in_cycle_normal_form = structural_connectivity_relation.is_cyclic_normal_form;

                    auto trace_manifold_graph = hg.get_trace_manifold_graph(structural_connectivity_relation);
                    int amount_of_nodes_in_trace_manifold_graph = 0;
                    for (auto traces_of_cycle : *trace_manifold_graph.trace_node_per_cycle)
                    {
                        amount_of_nodes_in_trace_manifold_graph += traces_of_cycle->size();
                    }
                    // printf("Amount of nodes in trace manifold graph: %d\n", amount_of_nodes_in_trace_manifold_graph);
                    // printf("Amount of edges in trace manifold graph: %d\n", trace_manifold_graph.edges->size());
                    int amount_of_edges_in_trace_manifold_graph = trace_manifold_graph.edges->size();

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
                    // printf("Amount of positions in all cycles: %d\n", amount_of_positions_in_all_cycles);
                    // printf("Amount of SCCs: %d\n", amount_of_SCCs);

                    // printf("Has overlapping cycles? %s\n", dg.contains_overlapping_cycles() ? "yes" : "no");
                    // printf("Proof width: %zu\n", max_amount_of_heights);

                    Heighted_graph::StructuralConnectivityRelation relation = hg.get_structural_connectivity_relation();
                    // printf("size of structural connectivity relation: %d\n", relation.relation->size());
                    int size_of_structural_connectivity_relation = relation.relation->size();
                    // printf("\n");

                    // printf("Is cyclic normal form? %s\n", relation.is_cyclic_normal_form ? "yes" : "no");

                    auto [FC_duration, FC_answer] = measure_flat_cycles_runtime(hg, runs_repetitions);
                    auto [DU_duration, DU_answer] = measure_descending_unicycles_runtime(hg, runs_repetitions);
                    auto [TM_duration, TM_answer] = measure_trace_manifold_runtime(hg, runs_repetitions);
                    auto [OR_duration, OR_answer] = measure_order_reduced_runtime(hg, runs_repetitions);

                    // printf("\n");
                    // fflush(stdout);

                    OutputRow row;
                    row.test_suite = test_suite;
                    row.graph_name = entry.path().filename();
                    row.width = max_amount_of_heights;
                    row.buds = buds;
                    row.edges = edges;
                    row.nodes = nodes;
                    row.amount_of_backedges = amount_of_backedges;
                    row.amounts_of_trace_manifold_graph_edges = amount_of_edges_in_trace_manifold_graph;
                    row.amounts_of_trace_manifold_graph_nodes = amount_of_nodes_in_trace_manifold_graph;
                    row.size_of_structural_connectivity_relation = size_of_structural_connectivity_relation;
                    row.amounts_of_positions_in_all_cycles = amount_of_positions_in_all_cycles;
                    row.amount_of_SCCs = amount_of_SCCs;
                    row.has_overlapping_cycles = has_overlapping_cycles;
                    row.is_in_cycle_normal_form = is_in_cycle_normal_form;
                    row.FC_duration_us = FC_duration;
                    row.FC_answer = FC_answer;
                    row.DU_duration_us = DU_duration;
                    row.DU_answer = DU_answer;
                    row.TM_duration_us = TM_duration;
                    row.TM_answer = TM_answer;
                    row.OR_duration_us = OR_duration;
                    row.OR_answer = OR_answer;

                    std::cout << row.to_csv_string();
                }
            }
        }
    }
}
