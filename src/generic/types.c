
#ifndef TYPES_HH_
#define TYPES_HH_

#include <set>
#include <map>

#define Map                     std::map
#define Pair                    std::pair
#define Int_SET                 std::set<int>
#define Int_pair                std::pair<int,int>
#define Int_pair_SET            std::set<std::pair<int,int>>
#define Sloped_Relation_SET     std::set<Sloped_relation*>

enum slope {Downward = 1,Stay = 0};

#endif