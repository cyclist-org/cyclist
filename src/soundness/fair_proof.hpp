#ifndef FAIR_PROOF_HH_
#define FAIR_PROOF_HH_

#include <unordered_set>
#include <unordered_map>
#include <vector>
#include <tuple>

#include <spot/twa/bdddict.hh>

#include "proof.hpp"

typedef int AcceptanceElem;
typedef std::unordered_set<AcceptanceElem> AcceptanceSet;

#define NO_ACC_ELEM 0

typedef std::tuple<AcceptanceElem, AcceptanceElem> FairnessConstraint;
namespace std {
  template<> struct hash< FairnessConstraint > {
    size_t operator()(FairnessConstraint const &key) const {
      int seed = 0;
      HASH_VAL(seed, AcceptanceElem, get< 0 >(key));
      HASH_VAL(seed, AcceptanceElem, get< 1 >(key));
      return seed;
    }
  };
}

class FairProof : public Proof {
protected:
  AcceptanceElem max_acc_elem;
  
  typedef std::unordered_map< Vertex, AcceptanceSet > AcceptanceSetMap;
  AcceptanceSetMap acc_set_map;
  
  std::unordered_set<FairnessConstraint> fairness_constraints;
  
public:
  FairProof(size_t maxv_l2);
  virtual ~FairProof() { dict->unregister_all_my_variables(this); }

  Vertex create_vertex();
  const AcceptanceSet get_acc_set_of_vertex(const Vertex & v) const;
  AcceptanceElem get_max_acc_elem() const { return max_acc_elem; }
  void set_fairness_constraint(const Vertex & v1, const Vertex & v2);
  virtual std::unordered_set<FairnessConstraint> get_fairness_constraints() const {return fairness_constraints; }
};
//==================================================================

#endif /* FAIR_PROOF_HH_ */
