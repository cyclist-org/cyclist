#include "fair_proof_aut.hpp"

#include <sstream>

// GhostState and ProofState functions removed because of duplication

//==================================================================
spot::twa_succ_iterator* FairProofAutomaton::succ_iter(const spot::state* local_state) const {
	const ProofState * ps = dynamic_cast< const ProofState *>(local_state);
	if(ps) return new ProofSuccIterator(*this, ps->vertex);

	const ProofGhostState * gs = dynamic_cast< const ProofGhostState *>(local_state);
	assert(gs);
	return new ProofGhostSuccIterator(*this);
}
//------------------------------------------------------------------
std::string FairProofAutomaton::format_state(const spot::state* state) const {
	const ProofGhostState * gs = dynamic_cast< const ProofGhostState *>(state);
	if(gs) return "";

	const ProofState * ps = dynamic_cast<const ProofState *>(state);
	assert(ps);

	std::stringstream ss;
	ss << 'S' << get_vertex_name(ps->vertex) << ' ';

	for(TagVector::const_iterator t=ps->tags.begin(); t!=ps->tags.end(); ++t) {
		if(t!=ps->tags.begin()) ss << ',';
		ss << "t_" << *t;
	}

	return ss.str();
}

//------------------------------------------------------------------
spot::acc_cond::mark_t ProofSuccIterator::acc() const {
  AcceptanceSet acc_set = proof.get_acc_set_of_vertex(vertex);
  if(acc_set.empty()) return spot::acc_cond::mark_t();
  return spot::acc_cond::mark_t(acc_set.begin(),acc_set.end());
}

void FairProofAutomaton::set_acceptance_condition() {
  std::stringstream acceptance_condition;
  std::unordered_set<FairnessConstraint> fairness_constraints = get_fairness_constraints();
  for(auto elem = fairness_constraints.begin(); elem != fairness_constraints.end(); ++elem) {
    if(elem != fairness_constraints.begin()) acceptance_condition << " & ";
    acceptance_condition << "(Fin(" << (std::get< 0 >(*elem))
			 << ") | Inf(" << (std::get< 1 >(*elem))
			 << "))" << std::flush;
  } 
  set_acceptance(get_max_acc_elem(),spot::acc_cond::acc_code(acceptance_condition.str().c_str()));
}
