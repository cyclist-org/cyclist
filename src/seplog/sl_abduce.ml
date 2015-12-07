module Proof = Proof.Make(Sl_seq)
module Rule = Proofrule.Make(Sl_seq)
module Prover = Prover.Make(Sl_seq)

exception Not_symheap = Sl_form.Not_symheap

let mk_axiom unify res =
  Rule.mk_axiom
    (fun ((g, g') as seq) ->
      let substs = unify g g' in
      let not_empty = not (Blist.is_empty substs) in 
      if not_empty then res := (seq, substs) ;
      Option.mk not_empty "Match")

let ruf_rl = Sl_rules.ruf_rl
let bounds_intro = Rule.mk_infrule Sl_rules.bounds_intro_rl

let rules = ref Rule.fail
let set_defs defs = 
  rules := Rule.first [
      bounds_intro ;
      Rule.mk_infrule (ruf_rl defs) ;
    ]

let maxdepth = ref 4

let set_depth d = maxdepth := d
let max_depth = !maxdepth

let abd_substs 
    ?(init_state=Sl_unify.Unidirectional.empty_state) ?(update_check=Fun._false) 
    ?(verify=Fun._true) ?(allow_frame=true) f f' =
  let result = ref ((f, f'), []) in
  let unifier f f' =
    try
      let (cs, h) = Sl_form.dest f in
      let cs = Ord_constraints.close cs in
      let (cs', h') = Sl_form.dest f' in
      let heap_unifier =
        if allow_frame then 
             Sl_heap.unify_partial ~update_check h' h
        else Sl_heap.classical_unify ~update_check h' h in
      Unification.backtrack
        (Sl_unify.Unidirectional.unify_tag_constraints ~update_check) cs' cs
        (heap_unifier
        (Sl_unify.Unidirectional.mk_verifier verify))
        init_state
    with Not_symheap -> [] in
  let rules = Rule.combine_axioms (mk_axiom unifier result) !rules in
  let proof = Prover.idfs 1 !maxdepth Rule.fail rules (f, f') in
  Option.mk (Option.is_some proof) !result

let abd_bi_substs 
    ?(init_state=Sl_unify.Bidirectional.empty_state) ?(update_check=Fun._false)
    ?(verify=Fun._true) ?(allow_frame=true) f f' =
  let result = ref ((f, f'), []) in
  let update_check upd = update_check (Pair.map Pair.swap upd) in
  let unifier f f' =
    try
      let (cs, h) = Sl_form.dest f in
      let cs = Ord_constraints.close cs in
      let (cs', h') = Sl_form.dest f' in
      let heap_unifier =
        if allow_frame then 
             Sl_heap.biunify_partial ~update_check h' h
        else Sl_heap.classical_biunify ~update_check h' h in
      Unification.backtrack
        (Sl_unify.Bidirectional.unify_tag_constraints ~update_check) cs' cs
        (heap_unifier
        (Sl_unify.Bidirectional.mk_verifier verify))
        (Pair.swap init_state)
    with Not_symheap -> [] in
  let rules = Rule.combine_axioms (mk_axiom unifier result) !rules in
  let proof = Prover.idfs 1 !maxdepth Rule.fail rules (f, f') in
  Option.map
    (fun _ -> 
      let (interpolant, substs) = !result in
      (interpolant, Blist.map Pair.swap substs))
    proof
