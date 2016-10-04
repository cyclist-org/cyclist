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
  Label lbl = proof.get_label_of_vertex(vertex);
  if(lbl==30) return spot::acc_cond::mark_t(1);
  else if (lbl==40) return spot::acc_cond::mark_t(2);
  return spot::acc_cond::mark_t();
}
