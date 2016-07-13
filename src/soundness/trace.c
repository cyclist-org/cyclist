#include "trace.hpp"

#include <cassert>
#include <sstream>
#include <spot/tl/defaultenv.hh>
#define ENV (spot::default_environment::instance())

//==================================================================
int TraceState::compare(const spot::state *other) const {
	// was dynamic_cast; caveat emptor
	const TraceState * s = reinterpret_cast<const TraceState *>(other);
	assert(s);

	// initial state is less than any other apart from itself
	if( initial() ) return s->initial() ? 0 : -1;

	// ditto in the opposite direction
	if( s->initial() ) return 1;

	if(vertex.id() < s->vertex.id()) return -1;
	if(vertex.id() > s->vertex.id()) return  1;
	if(tag < s->tag) return -1;
	if(tag > s->tag) return  1;
	return 0;
}
//------------------------------------------------------------------
size_t TraceState::hash() const {
	int seed = 0;
	HASH_VAL(seed, Vertex, vertex);
	HASH_VAL(seed, Tag, tag);
	return seed;
}
//==================================================================
bool TraceSuccIterator::first() {
	assert(state_info_vector.empty());
	const Proof & proof = automaton.proof;

	const VertexSet & successors =
			(state->initial()) ?
					proof.get_vertices() :
					proof.get_successors(state->vertex);

	state_info_vector.reserve(successors.size() * proof.get_max_tag());

	if(state->initial()) {
		state_info_vector.push_back( automaton.get_state( NO_VERTEX, NO_TAG ) );
	}

	for(VertexSet::const_iterator v=successors.begin();	v!=successors.end(); ++v ) {

		const TagVector & tset = proof.get_tags_of_vertex(*v);
		for(TagVector::const_iterator t=tset.begin(); t!=tset.end(); ++t) {

			if(!state->initial()) {
				if(!proof.trace_pair(state->vertex, *v, state->tag, *t)) continue;
			}

			state_info_vector.push_back( automaton.get_state( *v, *t ) );
		}
	}
	return !done();
}
//------------------------------------------------------------------
bdd TraceSuccIterator::cond() const {
	bdd c = state_info_vector.back()->vertex;
	if(c==NO_VERTEX)
		return bddtrue;
	else
		return c;
}
//------------------------------------------------------------------
spot::acc_cond::mark_t TraceSuccIterator::acc() const {
	TraceState * s = state_info_vector.back();
	return automaton.proof.progress_pair(
			state->vertex,
			s->vertex,
			state->tag,
			s->tag) ? automaton.acc_set : spot::acc_cond::mark_t();
}
//==================================================================
TraceAutomaton::TraceAutomaton(const Proof & p) :
	spot::twa(p.get_dict()), proof(p) {
	acc_set = set_buchi();
}
//------------------------------------------------------------------
TraceAutomaton::~TraceAutomaton() {
	for(StateMap::iterator i=state_map.begin(); i!=state_map.end(); ++i) {
		delete (i->second);
	}
}
//------------------------------------------------------------------
TraceState * TraceAutomaton::get_state(Vertex v, Tag t) const {
	StatePair p(v,t);
	StateMap::const_iterator i = state_map.find( p );
	if(i==state_map.end()) {
		TraceState * s = new TraceState(v,t);
		state_map[ p ] = s;
		return s;
	} else {
		assert( i->second != 0 );
		return i->second;
	}
}
//------------------------------------------------------------------
spot::twa_succ_iterator *TraceAutomaton::succ_iter(const spot::state *local_state) const {
	const TraceState * ts = dynamic_cast<const TraceState *>(local_state);
	assert(ts);
	return new TraceSuccIterator(*this, ts);
}
//------------------------------------------------------------------
std::string TraceAutomaton::format_state(const spot::state *state) const {
	const TraceState * ts = dynamic_cast<const TraceState *>(state);
	assert(ts);

	if( ts->initial() ) return "init";

	assert(ts->vertex != NO_VERTEX && ts->tag != NO_TAG);

	std::stringstream ss;
	ss << 'S' << proof.get_vertex_name(ts->vertex) << ",t" << ts->tag;

	return ss.str();
}
//std::string TraceAutomaton::transition_annotation(const spot::tgba_succ_iterator* t) const {
//	assert(false);
//	return "";
//}
//spot::state* TraceAutomaton::project_state(const spot::state* s, const spot::tgba* t) const {
//	assert(false);
//	return 0;
//}
//==================================================================




