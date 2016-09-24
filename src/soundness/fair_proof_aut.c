#include "fair_proof_aut.hpp"

#include <sstream>

//==================================================================
int ProofState::compare(const spot::state *other) const {
	const ProofGhostState * gs = dynamic_cast< const ProofGhostState * >(other);
	if(gs) return 1;

	const ProofState * ps = dynamic_cast< const ProofState * >(other);
	assert(ps);

	if(vertex.id() < ps->vertex.id()) return -1;
	if(vertex.id() > ps->vertex.id()) return 1;
	return 0;
}
//==================================================================
int ProofGhostState::compare(const spot::state *other) const {
	const ProofState * ps = dynamic_cast< const ProofState * >(other);
	if(ps) return -1;

	const ProofGhostState * gs = dynamic_cast< const ProofGhostState * >(other);
	assert(gs);

	return 0;
}
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
