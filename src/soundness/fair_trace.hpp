#ifndef FAIR_TRACE_HH_
#define FAIR_TRACE_HH_

#include <tuple>
#include <spot/twa/twa.hh>

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
class FairTraceAutomaton;
class FairTraceSuccIterator: public spot::twa_succ_iterator {
private:
	const FairTraceAutomaton & automaton;
	const TraceState * state;

	typedef std::vector< TraceState * > StateInfoVector;
	StateInfoVector state_info_vector;

public:
	FairTraceSuccIterator(const FairTraceAutomaton & ta, const TraceState * s) : automaton(ta), state(s) { }
	virtual ~FairTraceSuccIterator() {}

	virtual bool first();
	virtual bool next() { state_info_vector.pop_back(); return !done(); }
	virtual bool done() const { return state_info_vector.empty(); }
	virtual spot::state* dst() const { return state_info_vector.back(); }
	virtual bdd cond() const;
	virtual spot::acc_cond::mark_t acc() const;
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
class FairTraceAutomaton: public spot::twa {
private:
	const Proof & proof;
	bdd accept;
	spot::acc_cond::mark_t acc_set;

	typedef std::unordered_map< StatePair, TraceState * > StateMap;
	mutable StateMap state_map;

	TraceState * get_state(Vertex v, Tag t) const;

public:
	FairTraceAutomaton(const Proof & p);
	virtual ~FairTraceAutomaton();
	virtual spot::state* get_init_state() const { return get_state(NO_VERTEX, NO_TAG); }
	virtual spot::twa_succ_iterator* succ_iter(const spot::state* local_state) const;
	virtual spot::bdd_dict_ptr get_dict() const { return proof.get_dict(); }
	virtual std::string format_state(const spot::state* state) const;
//	virtual std::string transition_annotation(const spot::tgba_succ_iterator* t) const;
//	virtual spot::state* project_state(const spot::state* s, const spot::tgba* t) const;

	friend class FairTraceSuccIterator;
};
//==================================================================
#endif /* FAIR_TRACE_HH_ */
