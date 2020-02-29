open Lib
open Generic

module Proof = Proof.Make (Sl_seq)
module Rule = Proofrule.Make (Sl_seq)
module Seqtactics = Seqtactics.Make (Sl_seq)
module Prover = Prover.Make (Sl_seq)

exception Not_symheap = Sl_form.Not_symheap

let mk_axiom unify res =
  Rule.mk_axiom (fun ((g, g') as seq) ->
      let substs = unify g g' in
      let not_empty = not (Blist.is_empty substs) in
      if not_empty then res := (seq, substs) ;
      Option.mk not_empty "Match" )

let ruf_rl = Sl_rules.ruf_rl

let bounds_intro_rl = Sl_rules.bounds_intro_rl

let eq_ex_subst_rl = Sl_rules.eq_ex_subst_rule

let rhs_disj_to_symheaps = Rule.mk_infrule Sl_rules.rhs_disj_to_symheaps_rl

let simplify =
  Rule.mk_infrule
    (Seqtactics.relabel "Simplify"
       (Seqtactics.repeat (Seqtactics.first [bounds_intro_rl; eq_ex_subst_rl])))

let rules = ref Rule.fail

let set_defs defs = rules := Rule.mk_infrule (ruf_rl defs)

let maxdepth = ref 3

let set_depth d = maxdepth := d

let max_depth = !maxdepth

let abd_substs ?(used_tags = Tags.empty)
    ?(init_state = Sl_unify.Unidirectional.empty_state)
    ?(update_check = Fun._false) ?(verify = Fun._true) ?(allow_frame = true) f
    f' =
  let result = ref ((f, f'), []) in
  let unifier f f' =
    try
      let cs, h = Sl_form.dest f in
      let cs = Ord_constraints.close cs in
      let cs', h' = Sl_form.dest f' in
      let heap_unifier =
        if allow_frame then Sl_heap.unify_partial ~update_check
        else Sl_heap.classical_unify ~inverse:false ~update_check
      in
      let candidates =
        Unification.backtrack heap_unifier h' h
          (Sl_unify.Unidirectional.mk_verifier verify)
          init_state
      in
      let rec f (exc, inc) state =
        let g c =
          let () =
            debug (fun _ ->
                "Removing "
                ^ Ord_constraints.Elt.to_string c
                ^ " for tag constraint unification" )
          in
          let exc = Ord_constraints.add c exc in
          let inc = Ord_constraints.remove c inc in
          f (exc, inc) state
        in
        let res =
          Unification.backtrack
            (Sl_unify.Unidirectional.unify_tag_constraints ~update_check)
            inc cs
            (fun (trm_theta, tag_theta) ->
              let dom = Tagpairs.projectl tag_theta in
              let remaining = Tags.diff (Ord_constraints.tags exc) dom in
              let ex_subst = Tagpairs.mk_ex_subst used_tags remaining in
              let tag_theta' = Tagpairs.union tag_theta ex_subst in
              let exc' = Ord_constraints.subst_tags tag_theta' exc in
              if not (Ord_constraints.verify_schemas used_tags exc') then None
              else
                let univ_subst = Tagpairs.mk_free_subst used_tags remaining in
                let tag_theta' = Tagpairs.union tag_theta univ_subst in
                Option.pred verify (trm_theta, tag_theta') )
            state
        in
        let res =
          if not (Blist.is_empty res) then Option.some res
          else
            Ord_constraints.find_map
              (fun c -> Option.pred (fun x -> not (Blist.is_empty x)) (g c))
              inc
        in
        Option.dest Blist.empty Fun.id res
      in
      Blist.bind (f (Ord_constraints.empty, cs')) candidates
    with Not_symheap -> Blist.empty
  in
  let axiom = mk_axiom unifier result in
  let axiom =
    Rule.first
      [ Rule.sequence
          [Rule.attempt rhs_disj_to_symheaps; Rule.attempt simplify; axiom]
      ; axiom ]
  in
  let rules = Rule.combine_axioms axiom !rules in
  let proof = Prover.idfs 1 !maxdepth Rule.fail rules (f, f') in
  Option.mk (Option.is_some proof) !result

let abd_bi_substs ?(init_state = Sl_unify.Bidirectional.empty_state)
    ?(update_check = Fun._false) ?(verify = Fun._true) ?(allow_frame = true) f
    f' =
  let result = ref ((f, f'), []) in
  let update_check upd = update_check (Pair.map Pair.swap upd) in
  let unifier f f' =
    try
      let cs, h = Sl_form.dest f in
      let cs = Ord_constraints.close cs in
      let cs', h' = Sl_form.dest f' in
      let heap_unifier =
        if allow_frame then Sl_heap.biunify_partial ~update_check h' h
        else Sl_heap.classical_biunify ~update_check h' h
      in
      Unification.backtrack
        (Sl_unify.Bidirectional.unify_tag_constraints ~update_check)
        cs' cs
        (heap_unifier (Sl_unify.Bidirectional.mk_verifier verify))
        (Pair.swap init_state)
    with Not_symheap -> []
  in
  let axiom = mk_axiom unifier result in
  let axiom =
    Rule.first
      [ Rule.sequence
          [Rule.attempt rhs_disj_to_symheaps; Rule.attempt simplify; axiom]
      ; axiom ]
  in
  let rules = Rule.combine_axioms axiom !rules in
  let proof = Prover.idfs 1 !maxdepth Rule.fail rules (f, f') in
  Option.map
    (fun _ ->
      let interpolant, substs = !result in
      (interpolant, Blist.map Pair.swap substs) )
    proof
