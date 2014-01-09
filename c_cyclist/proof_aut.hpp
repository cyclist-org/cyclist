#ifndef PROOF_AUTOMATON_HH_
#define PROOF_AUTOMATON_HH_

#include <spot/tgba/tgba.hh>

#include "proof.hpp"

//==================================================================
class ProofState: public spot::state {
public:
	const Vertex vertex;
	const TagVector & tags;

	ProofState(const Vertex & v, const TagVector & ts) : vertex(v), tags(ts) {}

    virtual int compare(const spot::state* other) const;
	virtual size_t hash() const { return vertex.id(); }
	virtual spot::state* clone() const { return new ProofState(vertex, tags); }
};
//==================================================================
class ProofGhostState: public spot::state {
public:
    virtual int compare(const spot::state* other) const;
	virtual size_t hash() const { return 0; }
	virtual spot::state* clone() const { return new ProofGhostState(); }
};
//==================================================================
class ProofAutomaton: public spot::tgba, public Proof {
public:
	ProofAutomaton(size_t max_vertices_log2) : Proof(max_vertices_log2) {}

	virtual ~ProofAutomaton() {};
	virtual spot::state* get_init_state() const { return new ProofGhostState(); }
	virtual spot::bdd_dict* get_dict() const { return Proof::get_dict(); }
	virtual spot::tgba_succ_iterator* succ_iter(const spot::state* local_state,
			const spot::state* global_state = 0, const spot::tgba* global_automaton = 0) const;
	virtual std::string format_state(const spot::state* state) const;
//	virtual std::string transition_annotation(const spot::tgba_succ_iterator* t) const;
//	virtual spot::state* project_state(const spot::state* s, const spot::tgba* t) const;
	virtual bdd all_acceptance_conditions() const { return bddfalse; }
	virtual bdd neg_acceptance_conditions() const { return bddtrue; }

protected:
	virtual bdd compute_support_conditions(const spot::state* state) const { return bddtrue; }
	virtual bdd compute_support_variables(const spot::state* state) const { return bddtrue; }
};
//==================================================================
class ProofGhostSuccIterator: public spot::tgba_succ_iterator {
private:
	const ProofAutomaton & proof;
	bool finished;

public:
	ProofGhostSuccIterator(const ProofAutomaton & p) : proof(p), finished(false) {}

	virtual void first() { finished = false; }
	virtual void next() { finished = true; }
	virtual bool done() const { return finished; }
	virtual spot::state* current_state() const {
		Vertex v = proof.get_initial_vertex();
		return new ProofState(v, proof.get_tags_of_vertex(v) );
	}
	virtual bdd current_condition() const { return proof.get_initial_vertex(); }
	virtual bdd current_acceptance_conditions() const { return bddfalse; }
};
//==================================================================
class ProofSuccIterator: public spot::tgba_succ_iterator {
private:
	const ProofAutomaton & proof;
	Vertex vertex;
	VertexSet::const_iterator successor;

public:
	ProofSuccIterator(const ProofAutomaton & p, const Vertex & v) : proof(p), vertex(v) {}

	virtual void first() { successor = proof.get_successors(vertex).begin(); }
	virtual void next() { ++successor; }
	virtual bool done() const { return successor == proof.get_successors(vertex).end(); }
	virtual spot::state* current_state() const {
		return new ProofState(*successor, proof.get_tags_of_vertex(*successor) );
	}
	virtual bdd current_condition() const { return *successor; }
	virtual bdd current_acceptance_conditions() const { return bddfalse; }
};
//==================================================================
#endif /* PROOF_AUTOMATON_HH_ */
