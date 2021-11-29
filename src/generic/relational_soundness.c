#include <cassert>
#include <memory>

extern "C" {
#include <memory.h>
#include <mlvalues.h>
}

#include "heighted_graph.hpp"

static std::shared_ptr<Heighted_graph> hg = 0;

extern "C" void create_hgraph() {
  CAMLparam0();
  assert(hg == 0);
  hg = std::make_shared<Heighted_graph>();
  CAMLreturn0;
}

extern "C" void destroy_hgraph() {
  CAMLparam0();
  assert(hg);
  hg->clean();
  hg = 0;
  CAMLreturn0;
}

extern "C" void init_h_change() {
  CAMLparam0();
  assert(hg);
  hg->init_h_change_Ccl();
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

extern "C" value check_soundness_relational() {
  CAMLparam0();
  CAMLlocal1(v_res);

  assert(hg);
  // hg->compute_Ccl();
  bool retval = (hg->check_soundness_2());

  v_res = Val_bool(retval);
  CAMLreturn(v_res);
}