#include <cassert>
#include <map>
#include <spot/twaalgos/determinize.hh>
#include <spot/twaalgos/complement.hh>
#include <spot/twaalgos/totgba.hh>
#include <spot/twaalgos/copy.hh>
#include <spot/twaalgos/stutter.hh>
#include <spot/twa/twaproduct.hh>
#include <spot/twaalgos/gtec/gtec.hh>
#include <spot/twaalgos/dot.hh>
#include <iostream>

#include <string>
#include <spot/twaalgos/hoa.hh>
#include <spot/twa/bddprint.hh>


extern "C" {
#include <memory.h>
#include <mlvalues.h>
}

#include "fair_proof_aut.hpp"
#include "fair_trace.hpp"

static std::shared_ptr<FairProofAutomaton> proof = 0;
static std::map< int, Vertex > bdd_map;


void custom_print(std::ostream& out, spot::twa_graph_ptr& aut)
{
  // We need the dictionary to print the BDDs that label the edges
  const spot::bdd_dict_ptr& dict = aut->get_dict();

  // Some meta-data...
  out << "Acceptance: " << aut->get_acceptance() << '\n';
  out << "Number of sets: " << aut->num_sets() << '\n';
  out << "Number of states: " << aut->num_states() << '\n';
  out << "Number of edges: " << aut->num_edges() << '\n';
  out << "Initial state: " << aut->get_init_state_number() << '\n';
  out << "Atomic propositions:";
  for (spot::formula ap: aut->ap())
      out << ' ' << ap << " (=" << dict->varnum(ap) << ')';
  out << '\n';

  // Arbitrary data can be attached to automata, by giving them
  // a type and a name.  The HOA parser and printer both use the
  // "automaton-name" to name the automaton.
  if (auto name = aut->get_named_prop<std::string>("automaton-name"))
     out << "Name: " << *name << '\n';

  // For the following prop_*() methods, the return value is an
  // instance of the spot::trival class that can represent
  // yes/maybe/no.  These properties correspond to bits stored in the
  // automaton, so they can be queried in constant time.  They are
  // only set whenever they can be determined at a cheap cost: for
  // instance an algorithm that always produces deterministic automata
  // would set the deterministic property on its output.  In this
  // example, the properties that are set come from the "properties:"
  // line of the input file.
  out << "Deterministic: " << aut->prop_deterministic() << '\n';
  out << "Unambiguous: " << aut->prop_unambiguous() << '\n';
  out << "State-Based Acc: " << aut->prop_state_acc() << '\n';
  out << "Terminal: " << aut->prop_terminal() << '\n';
  out << "Weak: " << aut->prop_weak() << '\n';
  out << "Inherently Weak: " << aut->prop_inherently_weak() << '\n';
  out << "Stutter Invariant: " << aut->prop_stutter_invariant() << '\n';

  // States are numbered from 0 to n-1
  unsigned n = aut->num_states();
  for (unsigned s = 0; s < n; ++s)
    {
      out << "State " << s << ":\n";

      // The out(s) method returns a fake container that can be
      // iterated over as if the contents was the edges going
      // out of s.  Each of these edges is a quadruplet
      // (src,dst,cond,acc).  Note that because this returns
      // a reference, the edge can also be modified.
      for (auto& t: aut->out(s))
        {
          out << "  edge(" << t.src << " -> " << t.dst << ")\n    label = ";
          spot::bdd_print_formula(out, dict, t.cond);
          out << "\n    acc sets = " << t.acc << '\n';
        }
    }
}

extern "C" void create_fair_aut(value max_log2_states) {
	CAMLparam1(max_log2_states);
//	std::cerr << "create_aut " << Int_val(max_log2_states) << '\n';
	assert(proof==0);
	proof = std::make_shared<FairProofAutomaton>(Int_val(max_log2_states));
	CAMLreturn0;
}

extern "C" void destroy_fair_aut() {
	CAMLparam0();
//	std::cerr << "destroy_aut\n";
	assert(proof);
	proof = 0;
	bdd_map.clear();
	CAMLreturn0;
}

extern "C" void create_fair_vertex(value v_, value l_) {
	CAMLparam1(v_);
	assert(proof);
	Vertex v = proof->create_vertex();
	int id = Int_val(v_); //v.id();
	Label l = Int_val(l_);
	assert( bdd_map.find(id) == bdd_map.end() );
	bdd_map[id] = v;
	proof->label_vertex(v,l);
//	std:: cerr << "create_vertex " << id << '\n';
	CAMLreturn0;
}

extern "C" void tag_fair_vertex(value v_, value t_) {
	CAMLparam2(v_, t_);
	int v = Int_val(v_);
	Tag t = Int_val(t_);
	assert(proof);
	assert( bdd_map.find(v) != bdd_map.end() );
	proof->tag_vertex(bdd_map[v], t);
	CAMLreturn0;
}

extern "C" void set_fair_successor(value v1_, value v2_) {
	CAMLparam2(v1_, v2_);
	int v1 = Int_val(v1_);
	int v2 = Int_val(v2_);
	assert(proof);
	assert( bdd_map.find(v1) != bdd_map.end() );
	assert( bdd_map.find(v2) != bdd_map.end() );
	proof->set_successor(bdd_map[v1], bdd_map[v2]);
	CAMLreturn0;
}

extern "C" void set_fair_trace_pair(value v1_, value v2_, value t1_, value t2_) {
	CAMLparam4(v1_, v2_, t1_, t2_);
	int v1 = Int_val(v1_);
	int v2 = Int_val(v2_);
	Tag t1 = Int_val(t1_);
	Tag t2 = Int_val(t2_);
	assert(proof);
	assert( bdd_map.find(v1) != bdd_map.end() );
	assert( bdd_map.find(v2) != bdd_map.end() );
	proof->set_trace_pair(bdd_map[v1], bdd_map[v2], t1, t2);
	CAMLreturn0;
}

extern "C" void set_fair_progress_pair(value v1_, value v2_, value t1_, value t2_) {
	CAMLparam4(v1_, v2_, t1_, t2_);
	int v1 = Int_val(v1_);
	int v2 = Int_val(v2_);
	Tag t1 = Int_val(t1_);
	Tag t2 = Int_val(t2_);
	assert(proof);
	assert( bdd_map.find(v1) != bdd_map.end() );
	assert( bdd_map.find(v2) != bdd_map.end() );
	proof->set_progress_pair(bdd_map[v1], bdd_map[v2], t1, t2);
	CAMLreturn0;
}

extern "C" value check_fair_soundness() {
	CAMLparam0();
	CAMLlocal1(v_res);
	//	custom_print(std::cout,proof);
	spot::twa_graph_ptr proof_graph = copy(proof, spot::twa::prop_set::all());
	custom_print(std::cout,proof_graph);
	spot::const_twa_ptr ta = std::make_shared<FairTraceAutomaton>(*proof);
	spot::twa_graph_ptr graph = copy(ta, spot::twa::prop_set::all());
	custom_print(std::cout,graph);
	spot::twa_graph_ptr det = to_generalized_buchi(dtwa_complement(tgba_determinize(graph, false, true, true, spot::check_stutter_invariance(graph).is_true())));
	//spot::print_dot(std::cerr, ta);

	spot::const_twa_ptr product = std::make_shared<spot::twa_product>(proof, det);
	spot::couvreur99_check ec(product);
	std::shared_ptr<spot::emptiness_check_result> res = ec.check();

	bool retval = (res == 0);
	
	//std:: cout << "retval " << retval << '\n';

	v_res = Val_bool(retval);
	CAMLreturn(v_res);
}

extern "C" void set_initial_fair_vertex(value v_) {
	CAMLparam1(v_);
	int v = Int_val(v_);
//	std::cerr << "set_initial_vertex " << v << '\n';
	assert(proof);
	assert( bdd_map.find(v) != bdd_map.end() );
	proof->set_initial_vertex( bdd_map[v] );
	CAMLreturn0;
}
