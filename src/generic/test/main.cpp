#include <iostream>
#include "json.hpp"
#include "../types.c"
#include "../heighted_graph.hpp"
#include "../criterion.soundness.hpp"
#include "../criterion.flat_cycles.hpp"
#include "../criterion.descending_unicycles.hpp"
#include "../criterion.trace_manifold.hpp"
#include "../sledgehammer.hpp"
#include "heighted_graph_parser.cpp"

using json = nlohmann::json;

int usage(char* arg0) {
    std::cout
        << "Usage: "
        << arg0
        << " <filename>"
        << " <spec-string>"
        << std::endl
        << "<spec-string> ::= O [<order-id>] ([i]|([m][s])) [f][p]"
        << "([O]rder-reduced)"
        << std::endl
        << "                | H [<order-id>] ([i]|([m][s])) [f][p]"
        << "(Sledge[H]ammer)"
        << std::endl
        << "                | F ([i]|([m][s])) [f][p]"
        << "([F]loyd-Warshall-Kleene)"
        << std::endl
        << "                | V"
        << "([V]ertex-language Automaton encoding)"
        << std::endl
        << "                | S"
        << "([S]lope-language Automaton encoding)"
        << std::endl
        << "                | M"
        << "(Trace [Manifold])"
        << std::endl
        << "                | L"
        << "(F[L]at Cycles)"
        << std::endl
        << "                | D"
        << "([D]escending Unicycles)"
        << std::endl;
    return -1;
}

int main(int argc, char** argv) {
    if(argc > 1){

        std::string path = std::string("./data/");
        path = path + argv[1] + ".json";

        // Get JSON data
        std::ifstream graph_data(path.c_str());
        json graph;
        graph_data >> graph;

        // Create heighted graph object
        Heighted_graph hg = Heighted_graph(graph["Node"].size());
        parse_from_json(graph, hg);

        SoundnessCheckResult result;

        int opts = 0;

        auto start = std::chrono::system_clock::now();

        if (argc < 3) {
            (hg.order_reduced_check(Heighted_graph::GIVEN_ORDER, opts)) ?
                  result = SoundnessCheckResult::sound
                : result = SoundnessCheckResult::unsound;
        }
        else if (*argv[2] == '\0' || *argv[2] == 'O' || *argv[2] == 'H') {
            Heighted_graph::NODE_ORDER order;
            const char* spec = (*argv[2] == '\0') ? argv[2] : argv[2]+1;

            switch (*spec) {
                case '1': {
                    order = Heighted_graph::DEGREE_OUT_IN_ASC;
                    spec++;
                    break;
                }
                case '2': {
                    order = Heighted_graph::DEGREE_OUT_IN_DESC;
                    spec++;
                    break;
                }
                default: {
                    order = Heighted_graph::GIVEN_ORDER;
                    if (*spec != '\0') { spec++; }
                }
            }
            opts = Heighted_graph::parse_flags(spec);

            bool res;
            if(*argv[2] == 'O'){
                res = hg.order_reduced_check(order, opts);
            } else if (*argv[2] == 'H'){
                Sledgehammer sledgehammer(&hg, order, opts);
                res = sledgehammer.check_soundness();
            } else {
                return usage(argv[0]);
            }
            (res) ?
                  result = SoundnessCheckResult::sound
                : result = SoundnessCheckResult::unsound;
            // hg.print_statistics();
        } else {
            switch (*argv[2]) {
                case 'F': {
                    opts = Heighted_graph::parse_flags(argv[2]++);
                    (hg.fwk_check(opts)) ?
                          result = SoundnessCheckResult::sound
                        : result = SoundnessCheckResult::unsound;
                    break;
                }
                case 'V': {
                    hg.vla_automata_check() ?
                          result = SoundnessCheckResult::sound
                        : result = SoundnessCheckResult::unsound;
                    break;
                }
                case 'S': {
                    hg.sla_automata_check() ?
                          result = SoundnessCheckResult::sound
                        : result = SoundnessCheckResult::unsound;
                    break;
                }
                case 'M': {
                    TraceManifoldCriterion check(&hg);
                    result = check.check_soundness();
                    break;
                }
                case 'D': {
                    DescendingUnicyclesCriterion check(&hg);
                    result = check.check_soundness();
                    break;
                }
                case 'L': {
                    FlatCyclesCriterion check(&hg);
                    result = check.check_soundness();
                    break;
                }
                default: {
                    return usage(argv[0]);
                }
            }
        }

        // Output result
        switch (result) {
            case SoundnessCheckResult::sound:
                std::cout << "SOUND" << std::endl;
                break;
            case SoundnessCheckResult::unsound:
                std::cout << "UNSOUND" << std::endl;
                break;
            case SoundnessCheckResult::dontKnow:
                std::cout << "UNKNOWN" << std::endl;
                break;
            default:
                break;
        }

        auto end = std::chrono::system_clock::now();
        std::cout << std::chrono::duration_cast<std::chrono::microseconds>((end-start)).count() << "us" << std::endl;

        return result;
    }

    return usage(argv[0]);
}