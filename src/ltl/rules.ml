open Generic
open Lib

module Proof = Proof.Make(Seq)
module Rule = Proofrule.Make(Seq)
(* module Seqtactics = Seqtactics.Make(Seq) *)

(* let () = Rule.set_default_select_f 2 *)


(* Axiom *)

let axiom =
  Rule.mk_axiom
    (fun s ->
      Option.mk (Seq.is_axiomatic s) "Axiom")

(* Base Inference Rules *)

let disj =
  let rl seq =
    match (Seq.find_suchthat_opt Form.is_disj seq) with
    | None ->
      []
    | Some f ->
      let gamma = Seq.remove f seq in
      let (f_left, f_right) = Option.get (Form. dest_disj f) in
      let seq' = Seq.add_all [f_left; f_right] gamma in
      let valid_tps = Tagpairs.mk (Seq.tags gamma) in
      let prog_tps = Tagpairs.empty in
      [
        [ (seq', valid_tps, prog_tps) ], "Disj"
      ]
  in
  Rule.mk_infrule rl

let eventually =
  let rl seq =
    match (Seq.find_suchthat_opt Form.is_eventually seq) with
    | None ->
      []
    | Some f ->
      let gamma = Seq.remove f seq in
      let f' = Option.get (Form. dest_eventually f) in
      let seq' = Seq.add_all [f'; Form.mk_next f] gamma in
      let valid_tps = Tagpairs.mk (Seq.tags gamma) in
      let prog_tps = Tagpairs.empty in
      [
        [ (seq', valid_tps, prog_tps) ], "Eventually"
      ]
  in
  Rule.mk_infrule rl

let conj =
  let rl seq =
    match (Seq.find_suchthat_opt Form.is_conj seq) with
    | None ->
      []
    | Some f ->
      let gamma = Seq.remove f seq in
      let (f_left, f_right) = Option.get (Form. dest_conj f) in
      let left_premise = Seq.add f_left gamma in
      let right_premise = Seq.add f_right gamma in
      let valid_tps = Tagpairs.mk (Seq.tags gamma) in
      let prog_tps = Tagpairs.empty in
      [
        [ (left_premise, valid_tps, prog_tps)
        ; (right_premise, valid_tps, prog_tps)
        ] ,
        "Conj"
      ]
  in
  Rule.mk_infrule rl

let always =
  let rl seq =
    match (Seq.find_suchthat_opt Form.is_always seq) with
    | None ->
      []
    | Some f ->
      let tag = Option.get (Seq.get_tag f seq) in
      let gamma = Seq.remove f seq in
      let f' = Option.get (Form. dest_always f) in
      let f'' = Form.mk_next f in
      let left_premise = Seq.add f' gamma in
      let right_premise = Seq.add f'' gamma in
      let tag' = Option.get (Seq.get_tag f'' right_premise) in
      let valid_tps = Tagpairs.mk (Seq.tags gamma) in
      let prog_tps = Tagpairs.singleton (tag, tag') in
      let all_tps = Tagpairs.union valid_tps prog_tps in
      [
        [ (left_premise, valid_tps, Tagpairs.empty)
        ; (right_premise, all_tps, prog_tps)
        ] ,
        "Always"
      ]
  in
  Rule.mk_infrule rl

let next =
  let rl seq =
    if (not (Seq.exists Form.is_next seq)) then
      []
    else
      let premise =
        Seq.of_list
          (Seq.fold
            (fun f fs -> Option.dest fs (fun f -> f :: fs) (Form.dest_next f))
            seq
            []) in
      let tps =
        Seq.fold_with_tags
          (fun (t', f') tps ->
            let f = Form.mk_next f' in
            if not ((Form.is_traceable f) && (Form.is_traceable f')) then
              tps
            else
              let t = Option.get (Seq.get_tag f seq) in
              Tagpairs.add (t, t') tps)
          premise
          Tagpairs.empty in
      [
        [ (premise, tps, tps) ], "Next"
      ]
  in
  Rule.mk_infrule rl

(* Backlinking *)

let backlink =
  let select idx prf =
    !Rule.default_select_f idx prf in
  let mk_backlink bud companion =
    if (not (Seq.equal_upto_tags bud companion)) then
      []
    else
      let tps =
        Seq.fold_with_tags
          (fun (t, f) tps ->
            if (Form.is_traceable f) then
              let t' = Option.get (Seq.get_tag f companion) in
              Tagpairs.add (t, t') tps
            else
              tps)
          bud
          Tagpairs.empty in
      [ (tps, "Backlink") ]
  in
  Rule.mk_backrule false select mk_backlink

(* Weakening - guided by potential backlinks *)

let weaken idx prf =
  let rl seq =
    let weaken_wrt seq' =
      if (Seq.subset seq' seq) && not (Seq.equal_upto_tags seq' seq) then
        let premise =
          (* Assuming that Seq.inter returns only elements from its first arg,
             a more efficient solution would be: [Set.inter seq seq'].
             This is the case for the set implementation from Stdlib, but are
             we guaranteed to be using this? *)
          Seq.diff seq (Seq.diff seq seq') in
        (* We can just use the tags in the premise here, since the formulas it
           contains are guaranteed to values from [seq]. *)
        let valid_tps = Tagpairs.mk (Seq.tags premise) in
        Some ([(premise, valid_tps, Tagpairs.empty)], "Weaken")
      else
        None
      in
    let target_nodes = !Rule.default_select_f idx prf in
    let target_seqs =
      List.map (fun idx -> Proof.get_seq idx prf) target_nodes in
    List.filter_map weaken_wrt target_seqs
  in
  Rule.mk_infrule rl idx prf

(* (Multi)Cut - guided by potential backlinks *)

let cut idx prf =
  let rl seq =
    let cut_wrt seq' =
      let left_context =
          (* Assuming that Seq.inter returns only elements from its first arg,
             a more efficient solution would be: [Set.inter seq seq'].
             This is the case for the set implementation from Stdlib, but are
             we guaranteed to be using this? *)
          Seq.diff seq (Seq.diff seq seq') in
      let cut_formulas = Seq.to_list (Seq.diff seq' seq) in
      if Seq.is_empty left_context || List.is_empty cut_formulas then
        None
      else
        let left_tps = Tagpairs.mk (Seq.tags left_context) in
        let left_premise =
          (Seq.add_all cut_formulas left_context, left_tps, Tagpairs.empty) in
        let right_context = Seq.diff seq left_context in
        let right_tps = Tagpairs.mk (Seq.tags right_context) in
        let right_premises =
          List.map
            (fun f -> (Seq.add f right_context, right_tps, Tagpairs.empty))
            cut_formulas in
        let rulename =
          if List.length cut_formulas > 1 then "Multicut" else "Cut" in
        Some (left_premise :: right_premises, rulename)
    in
    let target_nodes = !Rule.default_select_f idx prf in
    let target_seqs =
      List.map (fun idx -> Proof.get_seq idx prf) target_nodes in
    List.filter_map cut_wrt target_seqs
  in
  Rule.mk_infrule rl idx prf

let use_cut = ref false

let cut =
  Rule.conditional
    (fun _ -> !use_cut)
    (Rule.compose_pairwise cut [ backlink ])

let use_cut b =
  use_cut := b

(* Inference Rule Tactics *)

let invertible_rules =
  [
    disj ;
    eventually ;
    conj ;
    always ;
  ]

let invertible_phase =
  let rules =
    Rule.conditional
      (fun seq -> not (Seq.is_axiomatic seq))
      (Rule.first invertible_rules) in
  Rule.compose rules (Rule.repeat rules)


(* The proof-search strategy *)

let axioms = ref axiom

let rules =
  ref
    (Rule.first [
      backlink ;
      Rule.sequence [ weaken ; backlink ] ;
      (* Rule.choice [ cut ; invertible_phase ] ; *)
      invertible_phase ;
      Rule.choice [
        next ;
        Rule.sequence [ next ; cut ] ;
      ] ;
    ])

