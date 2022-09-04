
#ifndef TYPES_HH_
#define TYPES_HH_

#include <set>
#include <map>
#include <unordered_map>
#include <list>
#include <vector>

#define NIL                 -1
#define Map                 std::map
#define Set                 std::set
#define Vec                 std::vector
#define Pair                std::pair
#define Int_SET             Set<int>
#define Int_pair            std::pair<int,int>
#define Int_pair_SET        std::set<std::pair<int,int>>
#define Sloped_Relation_SET Set<Sloped_relation*>
#define Relation_LIST       std::list<Sloped_relation*>


typedef std::list<int> NodeList;

enum slope {
  Undef = -1,
  Stay = 0,
  Downward = 1
};

/* Enumeration for comparing sloped relations */
enum comparison { 
  lt,     // Less
  eq,     // Equal
  gt,     // Greater 
  noncomp // Incomparable
};

enum SD_decrease_type {
  STD,
  XTD
};

#endif
