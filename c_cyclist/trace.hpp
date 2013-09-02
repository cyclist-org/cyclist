#ifndef TRACE_HH_
#define TRACE_HH_

#include <tuple>
#include <spot/tgba/public.hh>

#include "proof.hpp"

//==================================================================
class TraceState: public spot::state {
public:
	const Vertex vertex;
	const Tag tag;

	TraceState(const Vertex & v, const Tag & t) : vertex(v), tag(t) {}

	bool initial() const { return (vertex==NO_VERTEX) && (tag==NO_TAG); }

	virtual int compare(const spot::state* other) const;
	virtual size_t hash() const;

	// memory management happens at automaton level
	// thus these two functions do nothing
	virtual spot::state* clone() const { return (TraceState *) this; }
	virtual void destroy() const {}
};
//==================================================================
class TraceAutomaton;
class TraceSuccIterator: public spot::tgba_succ_iterator {
private:
	const TraceAutomaton & automaton;
	const TraceState * state;

	typedef std::vector< TraceState * > StateInfoVector;
	StateInfoVector state_info_vector;

public:
	TraceSuccIterator(const TraceAutomaton & ta, const TraceState * s) : automaton(ta), state(s) {}
	virtual ~TraceSuccIterator() {}

	virtual void first();
	virtual void next() { state_info_vector.pop_back(); }
	virtual bool done() const { return state_info_vector.empty(); }
	virtual spot::state* current_state() const { return state_info_vector.back(); }
	virtual bdd current_condition() const;
	virtual bdd current_acceptance_conditions() const;
};
//==================================================================
typedef std::pair< Vertex, Tag > StatePair;
namespace std {
	template<> struct hash< StatePair > {
		size_t operator()(StatePair const &key) const {
			int seed = 0;
			HASH_VAL(seed, Vertex, key.first);
			HASH_VAL(seed, Tag, key.second);
			return seed;
		}
	};
}
//==================================================================
class TraceAutomaton: public spot::tgba {
private:
	const Proof & proof;
	bdd accept;

	typedef std::unordered_map< StatePair, TraceState * > StateMap;
	mutable StateMap state_map;

	TraceState * get_state(Vertex v, Tag t) const;

public:
	TraceAutomaton(const Proof & p);
	virtual ~TraceAutomaton();
	virtual spot::state* get_init_state() const { return get_state(NO_VERTEX, NO_TAG); }
	virtual spot::tgba_succ_iterator* succ_iter(const spot::state* local_state,
			const spot::state* global_state = 0, const spot::tgba* global_automaton = 0) const;
	virtual spot::bdd_dict* get_dict() const { return proof.get_dict(); }
	virtual std::string format_state(const spot::state* state) const;
//	virtual std::string transition_annotation(const spot::tgba_succ_iterator* t) const;
//	virtual spot::state* project_state(const spot::state* s, const spot::tgba* t) const;
	virtual bdd all_acceptance_conditions() const { return accept; }
	virtual bdd neg_acceptance_conditions() const { return bdd_not(accept); }

protected:
	virtual bdd compute_support_conditions(const spot::state* state) const { return bddtrue; }
	virtual bdd compute_support_variables(const spot::state* state) const { return bddtrue; }

	friend class TraceSuccIterator;
};
//==================================================================
#endif /* TRACE_HH_ */
