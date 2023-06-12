#include <cassert>
#include <memory>

#include "heighted_graph.hpp"

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

extern "C" void add_node(value n_) {
  CAMLparam1(n_);
  assert(hg);
  int n = Int_val(n_);
  hg->add_node(n);
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

extern "C" void print_ccl() {
  CAMLparam0();
  assert(hg);
  hg->print_Ccl();
  CAMLreturn0;
}

extern "C" void print_statistics() {
  CAMLparam0();
  assert(hg);
  hg->print_statistics();
  CAMLreturn0;
}

extern "C" value relational_check(value opts_) {
  CAMLparam1(opts_);
  CAMLlocal1(v_res);

  assert(hg);
  int opts = Int_val(opts_);
  bool retval = (hg->relational_check(opts));

  v_res = Val_bool(retval);
  CAMLreturn(v_res);
}


extern "C" value automata_new_check() {
  CAMLparam0();
  CAMLlocal1(v_res);

  assert(hg);
  bool retval = (hg->check_automata_soundness());
  // bool retval = (aut_new->check_soundness());
  v_res = Val_bool(retval);
  CAMLreturn(v_res);
}

extern "C" value sd_check() {
  CAMLparam0();
  CAMLlocal1(v_res);
  assert(hg);
  bool retval = true;
  v_res = Val_bool(retval);
  CAMLreturn(v_res);
}

extern "C" value xsd_check() {
  CAMLparam0();
  CAMLlocal1(v_res);
  assert(hg);
  bool retval = true;
  v_res = Val_bool(retval);
  CAMLreturn(v_res);
}