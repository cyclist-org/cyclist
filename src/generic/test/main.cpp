#include <iostream>
#include "json.hpp"
#include "../types.c"
#include "../heighted_graph.hpp"

using json = nlohmann::json;

void parse_from_json(json &graph, Heighted_graph &hg){
    //====================== parse nodes
    std::cout << "Parsing Nodes: ";
    for( auto& element : graph["Node"] ){
        int id = element[0];
        for( int h : element[1] ){
            hg.add_height(id, h);
        }
    }
    std::cout << "Done! , Node size: " << hg.num_nodes() << std::endl;

    //====================== parse edges
    std::cout << "Parsing Edges: ";
    for( auto& element : graph["Edge"] ){
        int source = element[0][0];
        int sink = element[0][1];
        hg.add_edge(source, sink);
        for( auto& truple : element[1]){
            int source_h = truple[0];
            int sink_h = truple[1];
            slope s = static_cast<slope>(truple[2]);
            hg.add_hchange(source, source_h, sink, sink_h, s);
        }
    }
    std::cout << "Done! , Edge size: " << hg.num_edges() << std::endl;
}

int usage(char* arg0) {
    std::cout
        << "Usage: "
        << arg0
        << " <filename>"
        << " <spec-string>"
        << std::endl
        << "<spec-string> ::= O [<order-id>] ([i]|([m][s])) [f]"
        << "(Order-reduced)"
        << std::endl
        << "                | F ([i]|([m][s])) [f]"
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

        bool result;

        if (argc < 3) {
            result = hg.order_reduced_check(Heighted_graph::GIVEN_ORDER, opts);
        }
        else if (*argv[2] == '\0' || *argv[2] == 'O') {
            Heighted_graph::NODE_ORDER order;
            const char* spec = (*argv[2] == '\0') ? argv[2] : argv[2]++;
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
            result = hg.order_reduced_check(order, opts);
            // hg.print_Ccl();
            // hg.print_statistics();
        } else {
            switch (*argv[2]) {
                case 'F': {
                    opts = Heighted_graph::parse_flags(argv[2]++);
                    result = hg.fwk_check(opts);
                    break;
                }
                case 'V': {
                    result = hg.vla_automata_check();
                    break;
                }
                case 'S': {
                    result = hg.sla_automata_check();
                    break;
                }
                default: {
                    return usage(argv[0]);
                }
            }
        }

        // Output result
        if (result) {
            std::cout << "SOUND" << std::endl;
        } else {
            std::cout << "UNSOUND" << std::endl;
        }

        return !result;
    }

    return usage(argv[0]);
}