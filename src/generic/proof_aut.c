#include "proof_aut.hpp"

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
ProofAutomaton::ProofAutomaton(size_t max_vertices_log2) :
	spot::twa(spot::make_bdd_dict()), Proof(max_vertices_log2) {
	set_buchi();
	this->dict_ = Proof::get_dict();
	for (auto binding : this->dict_->var_map) {
		this->register_ap(binding.first.ap_name());
	}
}
//------------------------------------------------------------------
spot::twa_succ_iterator* ProofAutomaton::succ_iter(const spot::state* local_state) const {
	const ProofState * ps = dynamic_cast< const ProofState *>(local_state);
	if(ps) return new ProofSuccIterator(*this, ps->vertex);

	const ProofGhostState * gs = dynamic_cast< const ProofGhostState *>(local_state);
	assert(gs);
	return new ProofGhostSuccIterator(*this);
}
//------------------------------------------------------------------
std::string ProofAutomaton::format_state(const spot::state* state) const {
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
//std::string ProofAutomaton::transition_annotation(const spot::tgba_succ_iterator* t) const {
//	const ProofGhostSuccIterator * gs = dynamic_cast< const ProofGhostSuccIterator *>(t);
//	if(gs) return "";
//
//	const ProofSuccIterator * ps = dynamic_cast<const ProofSuccIterator *>(t);
//	assert(ps);
//
//	std::stringstream ss;
//	ss << 'S' << get_vertex_name(ps->current_condition());
//	return ss.str();
//}
//std::string ProofAutomaton::transition_annotation(const spot::tgba_succ_iterator* t) const {
//	assert(false);
//	return "";
//}
//spot::state* ProofAutomaton::project_state(const spot::state* s, const spot::tgba* t) const {
//	if( dynamic_cast< const ProofState * >(s) ) return s->clone();
//	if( dynamic_cast< const ProofGhostState * >(s) ) return s->clone();
//
//	//	const spot::state_product * sp = dynamic_cast< const spot::state_product * >(s);
////	*( (int *) 0) = 0;
////	const spot::state * l = sp->left();
////	if( dynamic_cast< const ProofState * >(r) ) return r->clone();
////	assert(false);
//	return 0;
//}
//==================================================================
