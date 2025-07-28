open Generic
open Lib

module Infrule = struct

  type t =
  | Axiom
  | ZeroLeft
  | ZeroRight
  | OneLeft of int list
  | OneRight
  | ConcatLeft of int
  | ConcatRight
  | ChoiceLeft of int
  | ChoiceRight
  | StarLeft of int
  | StarRight
  | Weaken
  | Cut


  let pp fmt r =
    let name =
      match r with
      | Axiom ->
        "Axiom"
      | ZeroLeft ->
        "0-left"
      | ZeroRight ->
        "0-right"
      | OneLeft _ ->
        "1-left"
      | OneRight ->
        "1-right"
      | ConcatLeft _ ->
        "·-left"
      | ConcatRight ->
        "·-right"
      | ChoiceLeft _ ->
        "+-left"
      | ChoiceRight ->
        "+-right"
      | StarLeft _ ->
        "*-left"
      | StarRight ->
        "*-right"
      | Weaken ->
        "Weaken"
      | Cut ->
        "Cut" in
    Format.pp_print_string fmt name

  let axiom = Axiom
  let zero_left = ZeroLeft
  let one_left idxs = OneLeft idxs
  let one_right = OneRight
  let concat_left idx = ConcatLeft idx
  let concat_right = ConcatRight
  let choice_left idx = ChoiceLeft idx
  let choice_right = ChoiceRight
  let star_left idx = StarLeft idx
  let star_right = StarRight
  let weaken = Weaken
  let cut = Cut

  let antecedent_ancestor_of idx (conc, prem) =
    function
    | ConcatLeft idx'
    | ChoiceLeft idx' ->
      if Int.(idx' < idx) then (idx - 1) else idx
    | StarLeft idx' ->
      if Int.(idx < idx') then
        idx
      else if (Seq.right_start conc > Seq.right_start prem) then
        idx + 1
      else if (idx' = idx) then
        idx
      else
        idx - 1
    | _ ->
      invalid_arg __FUNCTION__

end

module Proof = Proof.Make(Seq)(Infrule)
module Rule = Proofrule.Make(Seq)(Infrule)
module Node = Proofnode.Make(Seq)(Infrule)

(* Axioms *)

let atomic_axioms = ref false

let axiom =
  Rule.mk_axiom
    (fun s ->
      let is_axiomatic =
        Int.equal 2 (Seq.right_start s)
          &&
        match Seq.antecedent s, Seq.consequent s with
        | [e], [f] when Form.is_letter e || not !atomic_axioms ->
          Form.equal e f
        | _ ->
          false in
      Option.mk is_axiomatic Infrule.axiom)

let zero_left =
  Rule.mk_axiom
    (fun s ->
      Option.mk (Seq.exists_left Form.is_zero s) Infrule.zero_left)

let one_right =
  Rule.mk_axiom
    (fun s ->
      match Seq.antecedent s, Seq.consequent s with
      | [], [f] when Form.is_one f ->
        Some Infrule.one_right
      | _ ->
        None)

(* Base Inference Rules *)

let one_left =
  let rl seq =
    let premise = Seq.remove_left Form.is_one seq in
    if (Int.equal (Seq.right_start seq) (Seq.right_start premise)) then
      []
    else
      [
        [ premise, Tagpairs.mk (Seq.tags seq), Tagpairs.empty ],
            (Infrule.one_left (Seq.find_indices_left Form.is_one seq))
      ]
  in
  Rule.mk_infrule rl

(* Requires: [idx] is the index of a formula in the antecedent.
             For efficiency, we don't check that here, since this should only
             be called by the left-most left inference rule tactic. *)
let concat_left_at idx =
  let rl seq =
    match Form.dest_concat (Seq.nth seq idx) with
    | None ->
      []
    | Some (e, f) ->
      let premise = Seq.insert_many [e;f] idx (Seq.remove_at idx seq) in
      [
        [ (premise, Tagpairs.mk (Seq.tags seq), Tagpairs.empty) ],
            (Infrule.concat_left idx)
      ] in
  Rule.mk_infrule rl

(* Requires: [idx] is the index of a formula in the antecedent.
             For efficiency, we don't check that here, since this should only
             be called by the left-most left inference rule tactic. *)
let choice_left_at idx =
  let rl seq =
    match Form.dest_choice (Seq.nth seq idx) with
    | None ->
      []
    | Some (e, f) ->
      let premise_base = Seq.remove_at idx seq in
      let tps = Tagpairs.mk (Seq.tags seq) in
      [
        [
          (Seq.insert e idx premise_base, tps, Tagpairs.empty) ;
          (Seq.insert f idx premise_base, tps, Tagpairs.empty) ;
        ],
          (Infrule.choice_left idx)
      ] in
  Rule.mk_infrule rl

let choice_right =
  let rl seq =
    match (Seq.find_index_right Form.is_choice seq) with
    | None ->
      []
    | Some idx ->
      let (e, f) = Option.get (Form.dest_choice (Seq.nth seq idx)) in
      let premise = Seq.insert_many [e;f] idx (Seq.remove_at idx seq) in
      [
        [ (premise, Tagpairs.mk (Seq.tags seq), Tagpairs.empty) ],
            Infrule.choice_right
      ]
  in
  Rule.mk_infrule rl

(* Requires: [idx] is the index of a formula in the antecedent.
             For efficiency, we don't check that here, since this should only
             be called by the left-most left inference rule tactic. *)
let star_left_at idx =
  let rl seq =
    let e_star = Seq.nth seq idx in
    match Form.dest_star e_star with
    | None ->
      []
    | Some e ->
      let left_premise = Seq.remove_at idx seq in
      let right_premise = Seq.insert_many [e; e_star] idx left_premise in
      let left_tps = Tagpairs.mk (Seq.tags left_premise) in
      let prog_pair = (Seq.get_tag idx seq, Seq.get_tag (idx+1) right_premise) in
      let right_tps = Tagpairs.add prog_pair left_tps in
      [
        [
          (left_premise, left_tps, Tagpairs.empty) ;
          (right_premise, right_tps, Tagpairs.singleton prog_pair) ;
        ],
          (Infrule.star_left idx)
      ] in
  Rule.mk_infrule rl

let star_right =
  let rl seq =
    match (Seq.find_index_right Form.is_star seq) with
    | None ->
      []
    | Some idx ->
      let e_star = Seq.nth seq idx in
      let e = Option.get (Form.dest_star e_star) in
      let premise =
        Seq.insert_many
          [Form.one; Form.concat e e_star ]
          idx
          (Seq.remove_at idx seq) in
      [
        [ (premise, Tagpairs.mk (Seq.tags seq), Tagpairs.empty) ],
            Infrule.star_right
      ]
  in
  Rule.mk_infrule rl

(* Concatenation Right Rules *)

(* This invertible rule peels off the matching first letter from the antecedent
   and all consequent formulas. *)
let concat_right_first_letter =
  let rl seq =
    let consequent = List.map Form.factorise (Seq.consequent seq) in
    let (first, rest) =
      List.split (List.map (fun fs -> List.hd fs, List.tl fs) consequent) in
    if List.exists List.is_empty rest then
      []
    else
      match Seq.nth_opt seq 0 with
      | Some e when Form.is_letter e && List.for_all (Form.equal e) first ->
        let left_premise = Seq.with_consequent [e] (Seq.take_left 1 seq) in
        let left_tps = Tagpairs.mk (Seq.tags left_premise) in
        let right_premise =
          Seq.with_consequent
            (List.map Form.concatenate rest)
            (Seq.drop_left 1 seq) in
        let right_tps = Tagpairs.mk (Seq.tags right_premise) in
        [
          [ (left_premise, left_tps, Tagpairs.empty) ;
            (right_premise, right_tps, Tagpairs.empty) ; ],
              Infrule.concat_right
        ]
      | _ ->
        [] in
  Rule.mk_infrule rl

(* Start simple: when there is just one consequent formula *)
let concat_right_combs_singleton =
  let rl seq =
    let () = debug (fun _ -> "Trying " ^ __FUNCTION__) in
    match (Seq.consequent seq) with
    | [ f ] ->
      let antecedent_splits =
        (Seq.all_left_splits (Seq.with_consequent [] seq)) in
      let () =
        debug (fun _ ->
          Format.asprintf "antecedent splits: %a"
            (Format.pp_print_list (Pair.pp Seq.pp Seq.pp))
            antecedent_splits) in
      let antecedent_splits =
        List.map
          (Pair.map (fun seq -> (seq, Tagpairs.mk (Seq.tags seq))))
          (antecedent_splits) in
      let consequent_splits =
        List.map
          (Pair.map Form.concatenate)
          (Blist.all_splits ~allow_empty:false (Form.factorise f)) in
      let () =
        debug (fun _ ->
          Format.asprintf "consequent splits: %a"
            (Format.pp_print_list (Pair.pp Form.pp Form.pp))
            consequent_splits) in
      Blist.cartesian_map
        (fun ((gamma, gtps), (delta, dtps)) (f1, f2) ->
          [ (Seq.with_consequent [f1] gamma, gtps, Tagpairs.empty);
            (Seq.with_consequent [f2] delta, dtps, Tagpairs.empty) ],
              Infrule.concat_right)
        (antecedent_splits)
        (consequent_splits)
    | _ ->
      []  in
  Rule.mk_infrule rl

(* Weakening Tactics *)

(* Weaken away all consequent formulas that do not match the (single)
   antecendent formula. This is designed to be pre-composed with the [axiom]
   rule. *)
let wk_for_axiom =
  let rl seq =
    match (Seq.antecedent seq, Seq.consequent seq) with
    | ([e], [f]) when Form.equal e f ->
      []
    | ([e], _) ->
      let premise = Seq.filter_right (Form.equal e) seq in
      [
        [ (premise, Tagpairs.mk (Seq.tags premise), Tagpairs.empty) ],
            Infrule.weaken
      ]
    | (_, _) ->
      [] in
  Rule.mk_infrule rl

(* Weaken away all consequent formulas exception a single occurrence of
   [Form.one]. This can leave an empty consequent when there are no such
   occurrences. This is designed to be pre-composed with the [one_right]
   axiom. *)
let wk_leave_one =
  let rl seq =
    match (Seq.consequent seq) with
    | [f] when Form.is_one f ->
      []
    | _ ->
      let premise = Seq.filter_right Form.is_one seq in
      let premise = Seq.take_right 1 premise in
      [
        [ (premise, Tagpairs.mk (Seq.tags premise), Tagpairs.empty) ],
            Infrule.weaken
      ] in
  Rule.mk_infrule rl

(* Weaken away all consequent formulas. This is designed to be pre-composed with
   the [zero_left] axiom. *)
let wk_all =
  let rl seq =
    match (Seq.consequent seq) with
    | [] ->
      []
    | _ ->
      let premise = Seq.drop_right 0 seq in
      [
        [ (premise, Tagpairs.mk (Seq.tags premise), Tagpairs.empty) ],
            Infrule.weaken
      ] in
  Rule.mk_infrule rl

(* Weaken away any duplicated consequent formulas. The first occurrence of each
   formula is retained. *)
let wk_duplicates =
  let rec remove_dups acc =
    function
    | [] ->
      List.rev acc
    | f::fs ->
      if (List.exists (Form.equal f) acc) then
        remove_dups acc fs
      else
        remove_dups (f::acc) fs in
  let rl seq =
    let seq_conseq = Seq.consequent seq in
    let premise_conseq = remove_dups [] seq_conseq in
    if (Int.equal (List.compare_lengths seq_conseq premise_conseq) 0) then
      []
    else
      let premise = Seq.with_consequent premise_conseq seq in
      [
        [ (premise, Tagpairs.mk (Seq.tags seq), Tagpairs.empty) ],
            Infrule.weaken
      ]
  in
  Rule.mk_infrule rl

(* Weaken away any consequent formulas that cannot start with the first letter
   of the antecdent. This rule fails if the first antecedent formula is not a
   letter, or if the antecedent is empty. *)
let wk_non_matching =
  let rl seq =
    match (Seq.antecedent seq) with
    | c :: _ when Form.is_letter c ->
      let c = Option.get (Form.dest_letter c) in
      let consequent = Seq.consequent seq in
      let consequent' = List.filter (Form.can_start_with c) consequent in
      if (Int.equal (List.compare_lengths consequent consequent') 0) then
        (* If no formulas were removed, then fail *)
        []
      else
        let premise = Seq.with_consequent consequent' seq in
        [
          [ (premise, Tagpairs.mk (Seq.tags seq), Tagpairs.empty) ],
            Infrule.weaken
        ]
    | _ ->
      [] in
  Rule.mk_infrule rl

(* Cut *)

let cut_wrt seqs =
  let cut_wrt seq cut_seq =
    let cut_antecedent = Seq.antecedent cut_seq in
    let len_cut_antecedent = List.length cut_antecedent in
    match Blist.sublist_last_index (cut_antecedent) (Seq.antecedent seq) with
    | Some idx when len_cut_antecedent > 0 ->
      let cut_consequent = Seq.consequent cut_seq in
      let major_seq =
        Seq.with_consequent
          (cut_consequent)
          (Seq.drop_left idx (Seq.take_left (idx + len_cut_antecedent) seq)) in
      let major_tps = Tagpairs.mk (Seq.tags major_seq) in
      let minor_seq_base = Seq.remove_many_at idx len_cut_antecedent seq in
      let minor_tps = Tagpairs.mk (Seq.tags minor_seq_base) in
      let minors =
        List.map
          (fun f -> (Seq.insert f idx minor_seq_base, minor_tps, Tagpairs.empty))
          (cut_consequent) in
      Some ((major_seq, major_tps, Tagpairs.empty) :: minors, Infrule.cut)
    | _ ->
      None
  in
  Rule.mk_infrule (fun seq -> List.filter_map (cut_wrt seq) seqs)

(* Backlinking *)

let backlink =
  (* let select idx prf = !Rule.default_select_f idx prf in *)
  let rl bud companion =
    if (not (Seq.equal_upto_tags bud companion)) then
      []
    else
      [ Seq.zip_tags bud companion ]
  in
  (* Rule.mk_backrule false select mk_backlink *)
  Rule.mk_backrule false Rule.ancestor_nodes rl

let cut_backlink idx prf =
  (* Potential backlink targets are ancestral nodes that are not the conclusion
     of weakening rules. *)
  let targets =
    List.filter
      (fun idx ->
        match (Node.dest (Proof.find idx prf)) with
        | (_, None) | (_, Some Infrule.Weaken) -> false
        | _ -> true)
      (Rule.ancestor_nodes idx prf) in
  (* Sort the potential targets by consequent size, then antecendent size.
     We use a stable sort, so older nodes come first. *)
  let targets =
    List.stable_sort
      (fun idx idx' ->
        let seq = Proof.get_seq idx prf in
        let seq' = Proof.get_seq idx' prf in
        let result =
          (List.length (Seq.consequent seq)) - (List.length (Seq.consequent seq')) in
        if Int.equal result 0 then
          (Seq.right_start seq) - (Seq.right_start seq')
        else
          result)
      (targets) in
  let target_seqs =
    (* Filter sequents not containing an antecedent star formula, as
       backlinking guaranteed to fail for these. *)
    List.filter_map
      (fun idx ->
        let seq = Proof.get_seq idx prf in
        Option.mk (Seq.exists_left Form.is_star seq) seq)
      (targets) in
  Rule.compose_pairwise (cut_wrt target_seqs) [backlink] idx prf

(* Other tactics *)

let left_phase root_idx prf =
  (* Function to check whether the formula at position [f_idx] in
     node [node_idx] has already been unfolded along the path from
     node [root_idx]. *)
  let already_unfolded prf node_idx f_idx =
    let rec already_unfolded node_idx f_idx =
      if Int.equal node_idx root_idx then
        false
      else
        (* Get the parent node and ancestor formula.
           If the parent is the conclusion of StarLeft and the ancestor formula
           is the principal formula, then return false.
           Otherwise recurse *)
        let seq = Proof.get_seq node_idx prf in
        let (parent_idx, parent_node) =
           Option.get (Proof.find_parent node_idx prf) in
        let (parent_seq, rule) = Node.dest parent_node in
        let rule = Option.get rule in
        let ancestor_idx =
          Infrule.antecedent_ancestor_of f_idx (parent_seq, seq) rule in
        match rule with
        | StarLeft idx when Int.equal idx ancestor_idx ->
          true
        | _ ->
          already_unfolded parent_idx ancestor_idx in
    already_unfolded node_idx f_idx in
  (* This applies the left inference rule for the left-most non-atomic
     antecedent formula, as long as the formula, if a star formula, has not
     already been unfolded during the invocation of this tactic. *)
  let rl node_idx prf =
    let seq = Proof.get_seq node_idx prf in
    match (Seq.find_index_left (fun f -> not (Form.is_atom f)) seq) with
    | None ->
      Rule.fail node_idx prf
    | Some f_idx ->
      let f = Seq.nth seq f_idx in
      let rl =
        if (Form.is_choice f) then
          choice_left_at f_idx
        else if (Form.is_concat f) then
          concat_left_at f_idx
        else if (Form.is_star f && not (already_unfolded prf node_idx f_idx)) then
          star_left_at f_idx
        else
          Rule.fail in
      rl node_idx prf in
  (* Only apply the rule if the sequent cannot be closed by an axiom *)
  let rl_if_not_axiom =
    let nonatomic = not !atomic_axioms in
    Rule.conditional (fun s -> not (Seq.is_axiomatic ~nonatomic s)) rl in
  (* Repeat this as long as possible and finally apply One-left *)
  Rule.compose (Rule.repeat_one rl_if_not_axiom) (Rule.attempt one_left)
    root_idx prf

let right_invertible_phase =
  let weakenings = Rule.first [ wk_duplicates; wk_non_matching; ] in
  let right_rule =
    Rule.conditional
      (fun s -> not (Seq.is_axiomatic ~nonatomic:(not !atomic_axioms) s))
      (Rule.sequence [
          (Rule.first
            [ star_right; choice_right; concat_right_first_letter; ]) ;
          (Rule.repeat weakenings) ;
          (Rule.attempt backlink) ;
        ]) in
  Rule.first [
    Rule.repeat_one right_rule ;
    Rule.repeat_one weakenings ;
  ]

let search_step =
  Rule.non_empty
    (Rule.sequence [
      Rule.attempt left_phase ;
      Rule.attempt right_invertible_phase ;
      Rule.first [
          (* No-op if we can close using an axiom in the next step *)
          Rule.conditional
            (fun s -> Seq.is_axiomatic ~nonatomic:(not !atomic_axioms) s)
            (Rule.identity) ;
          Rule.attempt backlink ;
        ] ;
    ])

(* The proof-search strategy *)

let axioms =
  ref
    (Rule.first [
      Rule.compose (Rule.attempt wk_for_axiom) axiom ;
      Rule.compose (Rule.attempt wk_all) zero_left ;
      Rule.compose (Rule.attempt wk_leave_one) one_right ;
    ])

let rules =
  ref
    (Rule.first [
      backlink ;
      Rule.choice [ search_step; cut_backlink; concat_right_combs_singleton; ] ;
    ])