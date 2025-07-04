#include "../types.c"
#include "../heighted_graph.hpp"
#include "../cyclone.hpp"
#include <iostream>
#include <filesystem>
#include <regex>
#include <string>
#include "heighted_graph_parser.cpp"
#include "json.hpp"

using json = nlohmann::json;

int usage(char* arg0) {
    std::cout
        << "Usage: "
        << arg0
        << " <filename>"
        << " <spec-string>"
        << std::endl
        << "<spec-string> ::= O [<order-id>] ([i]|([m][s])) [f][p]"
        << "(Order-reduced)"
        << std::endl
        << "                | F ([i]|([m][s])) [f][p]"
        << "(Floyd-Warshall-Kleene)"
        << std::endl
        << "                | V"
        << "(Vertex-language Automaton encoding)"
        << std::endl
        << "                | S"
        << "(Slope-language Automaton encoding)"
        << std::endl;
    return -1;
}


int main(int argc, char **argv)
{
    if (argc > 1)
    {
        std::string path = argv[1];

        for (const auto &entry : std::filesystem::directory_iterator(path))
        {
            std::string path = entry.path();
            if (!entry.is_directory() && path.find(".json") != std::string::npos)
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

                bool result;

                int opts = 0;

                auto start = std::chrono::system_clock::now();

                if (argc < 3)
                {
                    result = hg.order_reduced_check(Heighted_graph::GIVEN_ORDER, opts);
                }
                else if (*argv[2] == '\0' || *argv[2] == 'O' || *argv[2] == 'C')
                {
                    Heighted_graph::NODE_ORDER order;
                    const char *spec = (*argv[2] == '\0') ? argv[2] : argv[2] + 1;

                    switch (*spec)
                    {
                    case '1':
                    {
                        order = Heighted_graph::DEGREE_OUT_IN_ASC;
                        spec++;
                        break;
                    }
                    case '2':
                    {
                        order = Heighted_graph::DEGREE_OUT_IN_DESC;
                        spec++;
                        break;
                    }
                    default:
                    {
                        order = Heighted_graph::GIVEN_ORDER;
                        if (*spec != '\0')
                        {
                            spec++;
                        }
                    }
                    }
                    opts = Heighted_graph::parse_flags(spec);

                    if (*argv[2] == 'O')
                    {
                        result = hg.order_reduced_check(order, opts);
                    }
                    else if (*argv[2] == 'C')
                    {
                        Cyclone cyclone(&hg, order, opts);
                        result = cyclone.check_soundness();
                    }
                    else
                    {
                        return usage(argv[0]);
                    }
                    // hg.print_statistics();
                }
                else
                {
                    switch (*argv[2])
                    {
                    case 'F':
                    {
                        opts = Heighted_graph::parse_flags(argv[2]++);
                        result = hg.fwk_check(opts);
                        break;
                    }
                    case 'V':
                    {
                        result = hg.vla_automata_check();
                        break;
                    }
                    case 'S':
                    {
                        result = hg.sla_automata_check();
                        break;
                    }
                    default:
                    {
                        return usage(argv[0]);
                    }
                    }
                }

                // Output result
                if (result)
                {
                    std::cout << "SOUND" << std::endl;
                }
                else
                {
                    std::cout << "UNSOUND" << std::endl;
                }
                if (result == is_sound){
                    std::cout << "TEST PASSED" << std::endl;
                } else {
                    std::cout << "TEST NOT PASSED" << std::endl;
                }

                auto end = std::chrono::system_clock::now();
                std::cout << "run on graph took " << std::chrono::duration_cast<std::chrono::microseconds>((end - start)).count() << "us" << std::endl;
            }
        }
    }
}