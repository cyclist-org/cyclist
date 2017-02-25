#include "fair_proof_aut.hpp"

#include <sstream>

//==================================================================
int FairProofState::compare(const spot::state *other) const {
	const FairProofGhostState * gs = dynamic_cast< const FairProofGhostState * >(other);
	if(gs) return 1;

	const FairProofState * ps = dynamic_cast< const FairProofState * >(other);
	assert(ps);

	if(vertex.id() < ps->vertex.id()) return -1;
	if(vertex.id() > ps->vertex.id()) return 1;
	return 0;
}
//==================================================================
int FairProofGhostState::compare(const spot::state *other) const {
	const FairProofState * ps = dynamic_cast< const FairProofState * >(other);
	if(ps) return -1;

	const FairProofGhostState * gs = dynamic_cast< const FairProofGhostState * >(other);
	assert(gs);

	return 0;
}

//==================================================================
spot::twa_succ_iterator* FairProofAutomaton::succ_iter(const spot::state* local_state) const {
	const FairProofState * ps = dynamic_cast< const FairProofState *>(local_state);
	if(ps) return new FairProofSuccIterator(*this, ps->vertex);

	const FairProofGhostState * gs = dynamic_cast< const FairProofGhostState *>(local_state);
	assert(gs);
	return new FairProofGhostSuccIterator(*this);
}
//------------------------------------------------------------------
std::string FairProofAutomaton::format_state(const spot::state* state) const {
	const FairProofGhostState * gs = dynamic_cast< const FairProofGhostState *>(state);
	if(gs) return "";

	const FairProofState * ps = dynamic_cast<const FairProofState *>(state);
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
spot::acc_cond::mark_t FairProofSuccIterator::acc() const {
  AcceptanceSet acc_set = proof.get_acc_set_of_vertex(vertex);
  if (proof.get_fairness_constraints().empty()) return proof.acc().all_sets();
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
			 << ")) & (Fin(" << (std::get< 1 >(*elem))
			 << ") | Inf(" << (std::get< 0 >(*elem))
			 << "))" << std::flush;
  }
  /* std::cout << "Setting acceptance condition: \"" << acceptance_condition.str() << "\"\n" ; */
  if (acceptance_condition.str().empty()) {
    set_buchi();
  } else {
    set_acceptance(get_max_acc_elem(),spot::acc_cond::acc_code(acceptance_condition.str().c_str()));
  }
}
