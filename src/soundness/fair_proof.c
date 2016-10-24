#include "fair_proof.hpp"

#include <sstream>
#include <cassert>

#include <spot/tl/defaultenv.hh>
#define ENV (spot::default_environment::instance())

//==================================================================
FairProof::FairProof(size_t maxv_l2) : Proof(maxv_l2),
  max_acc_elem(0) {  }
const AcceptanceSet FairProof::get_acc_set_of_vertex(const Vertex & v) const {
  AcceptanceSetMap::const_iterator it = acc_set_map.find(v);
  return it->second;
}
//------------------------------------------------------------------
Vertex FairProof::create_vertex() {
  Vertex v = Proof::create_vertex();
  acc_set_map[v];
  return v;
}
//------------------------------------------------------------------
void FairProof::set_fairness_constraint(const Vertex & v1, const Vertex & v2) {
  assert( vertices.find(v1) != vertices.end() );
  assert( vertices.find(v2) != vertices.end() );
  AcceptanceElem first_acc_elem = max_acc_elem++;
  AcceptanceElem second_acc_elem = max_acc_elem++;
  acc_set_map[v1].insert(first_acc_elem);
  acc_set_map[v2].insert(second_acc_elem);
  fairness_constraints.insert(FairnessConstraint(first_acc_elem, second_acc_elem));
}
