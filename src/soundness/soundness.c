#include <cassert>
#include <map>
#include <spot/tgba/tgbasafracomplement.hh>
#include <spot/tgba/tgbaproduct.hh>
#include <spot/tgbaalgos/gtec/gtec.hh>
#include <spot/tgbaalgos/dotty.hh>
#include <iostream>

extern "C" {
#include <memory.h>
#include <mlvalues.h>
}

#include "proof_aut.hpp"
#include "trace.hpp"

static ProofAutomaton * proof = 0;
static std::map< int, Vertex > bdd_map;

extern "C" void create_aut(value max_log2_states) {
	CAMLparam1(max_log2_states);
//	std::cerr << "create_aut " << Int_val(max_log2_states) << '\n';
	assert(proof==0);
	proof = new ProofAutomaton( Int_val(max_log2_states) );
	CAMLreturn0;
}

extern "C" void destroy_aut() {
	CAMLparam0();
//	std::cerr << "destroy_aut\n";
	assert(proof);
	delete proof;
	proof = 0;
	bdd_map.clear();
	CAMLreturn0;
}

//extern "C" value create_tag() {
//	CAMLparam0();
//	CAMLlocal1(v_res);
//	assert(proof);
//	v_res = Val_int(proof->create_tag());
//	CAMLreturn(v_res);
//}

extern "C" void create_vertex(value v_) {
	CAMLparam1(v_);
	assert(proof);
	Vertex v = proof->create_vertex();
	int id = Int_val(v_); //v.id();
	assert( bdd_map.find(id) == bdd_map.end() );
	bdd_map[id] = v;
//	std:: cerr << "create_vertex " << id << '\n';
	CAMLreturn0;
}

extern "C" void tag_vertex(value v_, value t_) {
	CAMLparam2(v_, t_);
	int v = Int_val(v_);
	Tag t = Int_val(t_);
	assert(proof);
	assert( bdd_map.find(v) != bdd_map.end() );
	proof->tag_vertex(bdd_map[v], t);
	CAMLreturn0;
}

extern "C" void set_successor(value v1_, value v2_) {
	CAMLparam2(v1_, v2_);
	int v1 = Int_val(v1_);
	int v2 = Int_val(v2_);
	assert(proof);
	assert( bdd_map.find(v1) != bdd_map.end() );
	assert( bdd_map.find(v2) != bdd_map.end() );
	proof->set_successor(bdd_map[v1], bdd_map[v2]);
	CAMLreturn0;
}

extern "C" void set_trace_pair(value v1_, value v2_, value t1_, value t2_) {
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

extern "C" void set_progress_pair(value v1_, value v2_, value t1_, value t2_) {
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

extern "C" value check_soundness() {
	CAMLparam0();
	CAMLlocal1(v_res);
//	spot::dotty_reachable(std::cout, proof);

	TraceAutomaton ta(*proof);
	spot::tgba_safra_complement complement(&ta);
	spot::tgba_product product(proof, &complement);
	spot::couvreur99_check ec(&product);
	spot::emptiness_check_result * res = ec.check();

	bool retval = (res == 0);

	if(res) delete res;

	v_res = Val_bool(retval);
	CAMLreturn(v_res);
}

extern "C" void set_initial_vertex(value v_) {
	CAMLparam1(v_);
	int v = Int_val(v_);
//	std::cerr << "set_initial_vertex " << v << '\n';
	assert(proof);
	assert( bdd_map.find(v) != bdd_map.end() );
	proof->set_initial_vertex( bdd_map[v] );
	CAMLreturn0;
}
