#include "fair_trace.hpp"

#include <cassert>
#include <sstream>
#include <spot/tl/defaultenv.hh>
#define ENV (spot::default_environment::instance())

//TraceState functions removed becuase of duplication

//==================================================================
bool FairTraceSuccIterator::first() {
  assert(state_info_vector.empty());
  const FairProof & proof = automaton.proof;

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
bdd FairTraceSuccIterator::cond() const {
  bdd c = state_info_vector.back()->vertex;
  if(c==NO_VERTEX)
    return bddtrue;
  else
    return c;
}
//------------------------------------------------------------------
spot::acc_cond::mark_t FairTraceSuccIterator::acc() const {

  std::vector<int> accepting_sets;
  
  TraceState * s = state_info_vector.back();
  if (automaton.proof.progress_pair(state->vertex,
				s->vertex,
				state->tag,
				    s->tag))
    accepting_sets.push_back(2);

  Label lbl = automaton.proof.get_label_of_vertex(state->vertex);
  if(lbl==30) accepting_sets.push_back(0);
  else if (lbl==40) accepting_sets.push_back(1);

  return spot::acc_cond::mark_t(std::begin(accepting_sets),std::end(accepting_sets));
  
}
//==================================================================
FairTraceAutomaton::FairTraceAutomaton(const FairProof & p) : proof(p), spot::twa(p.get_dict()) {
  set_generalized_buchi(3); // TODO: this will most likely change to a more complex condition
}
//------------------------------------------------------------------
FairTraceAutomaton::~FairTraceAutomaton() {
  for(StateMap::iterator i=state_map.begin(); i!=state_map.end(); ++i) {
    delete (i->second);
  }
}
//------------------------------------------------------------------
TraceState * FairTraceAutomaton::get_state(Vertex v, Tag t) const {
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
spot::twa_succ_iterator *FairTraceAutomaton::succ_iter(const spot::state *local_state) const {
  const TraceState * ts = dynamic_cast<const TraceState *>(local_state);
  assert(ts);
  return new FairTraceSuccIterator(*this, ts);
}
//------------------------------------------------------------------
std::string FairTraceAutomaton::format_state(const spot::state *state) const {
  const TraceState * ts = dynamic_cast<const TraceState *>(state);
  assert(ts);

  if( ts->initial() ) return "init";

  assert(ts->vertex != NO_VERTEX && ts->tag != NO_TAG);

  std::stringstream ss;
  ss << 'S' << proof.get_vertex_name(ts->vertex) << ",t" << ts->tag;

  return ss.str();
}
