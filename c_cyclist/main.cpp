#include <stdio.h>
#include <stdlib.h>

#include <spot/tgba/public.hh>
#include <spot/tgbaalgos/dotty.hh>
#include <spot/tgbaalgos/gtec/gtec.hh>
#include <spot/tgbaalgos/replayrun.hh>
#include <spot/tgba/tgbasafracomplement.hh>
#include <spot/tgba/tgbakvcomplement.hh>
#include <spot/tgba/tgbaproduct.hh>
#include <spot/tgbaalgos/projrun.hh>
#include <spot/misc/version.hh>

#include "proof_aut.hpp"
#include "trace.hpp"

//void test() {
	//	cyc_automaton c;
	//
	////	spot::dotty_reachable(std::cout, &c);
	//	spot::emptiness_check * ec = new spot::couvreur99_check(&c);
	//	spot::emptiness_check_result * res = ec->check();
	//	ec->print_stats(std::cout);
	//	if (res) {
	//		std::cout << "an accepting run exists" << std::endl;
	//		spot::tgba_run* run = res->accepting_run();
	//		if (run)
	//		{
	//			spot::print_tgba_run(std::cout, &c, run);
	//			delete run;
	//		}
	//	} else {
	//		std::cout << "no accepting run found" << std::endl;
	//	}
//}

//class TestProof: public ProofAutomaton {
//public:
////	Vertex vt1, vt3;
////	Tag tt1;
//
//	TestProof() : ProofAutomaton(3) {
//		Tag t1 = create_tag();
//		Tag t2 = create_tag();
//
//		Vertex v1 = create_vertex();
//		set_initial_vertex(v1);
//		tag_vertex(v1, t1);
//		tag_vertex(v1, t2);
//
////		vt1 = v1;
////		tt1 = t1;
//
//		Vertex v2 = create_vertex();
//		tag_vertex(v2, t2);
//
//		Vertex v3 = create_vertex();
//		tag_vertex(v3, t1);
//		tag_vertex(v3, t2);
//
////		vt3 = v3;
//
//		Vertex v4 = create_vertex();
//		tag_vertex(v4, t1);
//		tag_vertex(v4, t2);
//
//		Vertex v5 = create_vertex();
//		tag_vertex(v5, t2);
//
//		set_successor(v1, v2);
//		set_successor(v1, v3);
//		set_successor(v3, v5);
//		set_successor(v3, v4);
//		set_successor(v4, v1);
//
//
//		set_trace_pair(v1, v2, t2, t2);
//
//		set_trace_pair(v1, v3, t1, t1);
//		set_trace_pair(v1, v3, t2, t2);
//
//		set_trace_pair(v3, v4, t1, t1);
//		set_trace_pair(v3, v4, t2, t2);
//
//		set_trace_pair(v3, v5, t2, t2);
//
//		set_trace_pair(v4, v1, t1, t1);
//		set_trace_pair(v4, v1, t2, t2);
//
////		set_progress_pair(v1, v3, t1, t1);
//
//	}
//};

class TestProof: public ProofAutomaton {
public:
	TestProof() : ProofAutomaton(3) {
		Tag t1 = 1; //create_tag();
		Tag t2 = 2; //create_tag();

		Vertex v1 = create_vertex();
		set_initial_vertex(v1);
		tag_vertex(v1, t1);
		tag_vertex(v1, t2);

		Vertex v2 = create_vertex();
		tag_vertex(v2, t1);

		Vertex v3 = create_vertex();
		tag_vertex(v3, t1);

		Vertex v4 = create_vertex();
		tag_vertex(v4, t2);

		Vertex v5 = create_vertex();
		tag_vertex(v5, t2);

		Vertex v6 = create_vertex();

		set_successor(v1, v2);
		set_successor(v2, v3);
		set_successor(v3, v4);
		set_successor(v1, v4);
		set_successor(v4, v5);
		set_successor(v5, v2);
		set_successor(v4, v6);

		set_trace_pair(v1, v2, t1, t1);

		set_trace_pair(v2, v3, t1, t1);

		set_trace_pair(v3, v4, t1, t2);

		set_trace_pair(v4, v5, t2, t2);

		set_trace_pair(v5, v2, t2, t1);

		set_progress_pair(v2, v3, t1, t1);
//		set_progress_pair(v4, v5, t2, t2);

	}
};

void display(const spot::tgba * aut) {
	spot::dotty_reachable(std::cout, aut);
}

void check_empty(spot::tgba * aut, spot::tgba * proj) {
	spot::emptiness_check * ec = new spot::couvreur99_check(aut);
	spot::emptiness_check_result * res = ec->check();
//	ec->print_stats(std::cout);
	if (res) {
		spot::tgba_run* run = res->accepting_run();
		assert(run);
		spot::replay_tgba_run(std::cout, aut, run, false);

//		spot::tgba_run * projrun = spot::project_tgba_run(aut, proj, run);
//		assert(projrun);

//		spot::replay_tgba_run(std::cout, proj, projrun, true);
		delete run;
//		delete projrun;
		delete res;
	} else {
		std::cout << "no accepting run found" << std::endl;
	}
	delete ec;
}



int main(void) {
	std::cout << spot::version() << '\n';

	TestProof tp;
//	display(&tp);
	TraceAutomaton ta(tp);
	display(&ta);
//	check_empty(&ta);

	spot::tgba_safra_complement complement(&ta);
//	spot::tgba_kv_complement complement(&ta);
//	display(&complement);
//	check_empty(&complement);


	spot::tgba_product product(&tp, &complement);
//	display(&product);
//	check_empty(&product, &tp);
//	for(int i=0; i<16; i++){
//		int tmp = i;
//		for(int j=0; j<4; j++) {
//			std::cerr << ((tmp % 2) ? '1' : '0');
//			tmp>>=1;
//		}
//		std::cerr<<'\n';
//	}

//	check_empty(&complement);

	return 0;
}
