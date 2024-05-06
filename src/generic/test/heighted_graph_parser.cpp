#include "json.hpp"
#include "../types.c"
#include "../heighted_graph.hpp"

using json = nlohmann::json;

void parse_from_json(json &graph, Heighted_graph &hg)
{
    //====================== parse nodes
    std::cout << "Parsing Nodes: ";
    for (auto &element : graph["Node"])
    {
        int id = element[0];
        for (int h : element[1])
        {
            hg.add_height(id, h);
        }
    }
    std::cout << "Done! , Node size: " << hg.num_nodes() << std::endl;

    //====================== parse edges
    std::cout << "Parsing Edges: ";
    for (auto &element : graph["Edge"])
    {
        int source = element[0][0];
        int sink = element[0][1];
        hg.add_edge(source, sink);
        for (auto &truple : element[1])
        {
            int source_h = truple[0];
            int sink_h = truple[1];
            slope s = static_cast<slope>(truple[2]);
            hg.add_hchange(source, source_h, sink, sink_h, s);
        }
    }
    std::cout << "Done! , Edge size: " << hg.num_edges() << std::endl;
}