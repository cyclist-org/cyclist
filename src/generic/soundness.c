#include <cassert>
#include <memory>

#include "heighted_graph.hpp"
#include "sledgehammer.hpp"

extern "C" {
#include <memory.h>
#include <mlvalues.h>
}

static std::shared_ptr<Heighted_graph> hg = 0;

extern "C" void create_hgraph(value max_nodes_) {
  CAMLparam1(max_nodes_);
  assert(hg == 0);
  int max_nodes = Int_val(max_nodes_);
  hg = std::make_shared<Heighted_graph>(max_nodes);
  CAMLreturn0;
}

extern "C" void destroy_hgraph() {
  CAMLparam0();
  assert(hg);
  hg = 0;
  CAMLreturn0;
}

extern "C" void add_node(value n_, value is_bud_) {
  CAMLparam2(n_, is_bud_);
  assert(hg);
  int n = Int_val(n_);
  int is_bud = Bool_val(is_bud_);
  hg->add_node(n, is_bud);
  CAMLreturn0;
}

extern "C" void add_height(value n_, value h_) {
  CAMLparam2(n_, h_);
  assert(hg);
  int n = Int_val(n_);
  int h = Int_val(h_);
  hg->add_height(n, h);
  CAMLreturn0;
}

extern "C" void add_edge(value n1_, value n2_) {
  CAMLparam2(n1_, n2_);
  assert(hg);
  int n1 = Int_val(n1_);
  int n2 = Int_val(n2_);
  hg->add_edge(n1, n2);
  CAMLreturn0;
}

extern "C" void add_stay(value src_, value src_h_, value snk_, value snk_h_) {
  CAMLparam4(src_, src_h_, snk_, snk_h_);
  assert(hg);
  int src = Int_val(src_);
  int src_h = Int_val(src_h_);
  int snk = Int_val(snk_);
  int snk_h = Int_val(snk_h_);
  hg->add_stay(src, src_h, snk, snk_h);
  CAMLreturn0;
}

extern "C" void add_decr(value src_, value src_h_, value snk_, value snk_h_) {
  CAMLparam4(src_, src_h_, snk_, snk_h_);
  assert(hg);
  int src = Int_val(src_);
  int src_h = Int_val(src_h_);
  int snk = Int_val(snk_);
  int snk_h = Int_val(snk_h_);
  hg->add_decrease(src, src_h, snk, snk_h);
  CAMLreturn0;
}

extern "C" void print_statistics() {
  CAMLparam0();
  assert(hg);
  hg->print_statistics();
  CAMLreturn0;
}

extern "C" value order_reduced_check(value node_order_, value opts_) {
  CAMLparam2(node_order_, opts_);
  CAMLlocal1(v_res);

  int node_order = Int_val(node_order_);
  int opts = Int_val(opts_);

  assert(hg);
  assert(Heighted_graph::is_valid_node_order(node_order));

  Heighted_graph::NODE_ORDER ord =
    static_cast<Heighted_graph::NODE_ORDER>(node_order);

  bool should_halt = false;
  bool retval = (hg->order_reduced_check(ord, opts, &should_halt));

  v_res = Val_bool(retval);
  CAMLreturn(v_res);
}

extern "C" value fwk_check(value opts_) {
  CAMLparam1(opts_);
  CAMLlocal1(v_res);

  assert(hg);
  int opts = Int_val(opts_);

  bool should_halt = false;
  bool retval = (hg->fwk_check(opts, &should_halt));

  v_res = Val_bool(retval);
  CAMLreturn(v_res);
}

extern "C" value sla_automata_check() {
  CAMLparam0();
  CAMLlocal1(v_res);

  assert(hg);
  bool retval = (hg->sla_automata_check());
  v_res = Val_bool(retval);
  CAMLreturn(v_res);
}

extern "C" value vla_automata_check() {
  CAMLparam0();
  CAMLlocal1(v_res);

  assert(hg);
  bool retval = (hg->vla_automata_check());
  v_res = Val_bool(retval);
  CAMLreturn(v_res);
}

extern "C" value sledgehammer_check(value node_order_, value opts_) {
  CAMLparam2(node_order_, opts_);
  CAMLlocal1(v_res);

  int node_order = Int_val(node_order_);
  int opts = Int_val(opts_);

  assert(hg);
  assert(Heighted_graph::is_valid_node_order(node_order));

  Heighted_graph::NODE_ORDER ord =
    static_cast<Heighted_graph::NODE_ORDER>(node_order);

  Sledgehammer sledgehammer(hg.get(), ord, opts);

  bool retval = (sledgehammer.check_soundness());

  v_res = Val_bool(retval);
  CAMLreturn(v_res);
}


// extern "C" value sd_check() {
//   CAMLparam0();
//   CAMLlocal1(v_res);
//   assert(hg);
//   bool retval = true;
//   v_res = Val_bool(retval);
//   CAMLreturn(v_res);
// }

// extern "C" value xsd_check() {
//   CAMLparam0();
//   CAMLlocal1(v_res);
//   assert(hg);
//   bool retval = true;
//   v_res = Val_bool(retval);
//   CAMLreturn(v_res);
// }