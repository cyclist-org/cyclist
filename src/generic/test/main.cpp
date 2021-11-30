#include <iostream>
#include "json.hpp"
#include "../types.c"
#include "../heighted_graph.hpp"

using json = nlohmann::json;

void parse_from_json(std::string &path, Heighted_graph &hg){
    std::ifstream graph_data(path.c_str());
    json graph;
    graph_data >> graph;

    //====================== parse nodes
    std::cout << "Parsing Nodes: ";
    for( auto& element : graph["Node"] ){
        int id = element[0];
        if( id > hg.get_node_size() ) hg.set_node_size(id); 
        for( int h : element[1] ){
            hg.add_height(id, h);
        }
    }
    std::cout << "Done! , Node size: " << hg.num_nodes() << " max node id: " << hg.get_node_size() << std::endl;

    //====================== parse edges
    hg.init();
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

int main(int argc, char** argv) {
    if(argc > 1){
        std::string path = std::string("./data/");
        path = path + argv[1] + ".json";
        int opts = 0;
        if(const char* flags = std::getenv("FLAGS")) {
            opts = atoi(flags);
        }
        Heighted_graph hg = Heighted_graph();
        parse_from_json(path, hg);
        bool result = hg.check_soundness(opts);
        // hg.print_Ccl();
        if( result ){
            std::cout << "SOUND" << std::endl;
        }
        else {
            std::cout << "UNSOUND" << std::endl;
        }
        hg.clean();
        return !result;
    }

    std::cout << "Provide test file name!\n";
    return -1;
}