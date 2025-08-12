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

(* Applies the left rule for the left-most antecedent concatenation formula *)
let concat_left node_idx prf =
  let seq = Proof.get_seq node_idx prf in
  match (Seq.find_index_left Form.is_concat seq) with
  | None ->
    Rule.fail node_idx prf
  | Some seq_idx ->
    concat_left_at seq_idx node_idx prf

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

(* Applies the left rule for the left-most antecdent choice formula *)
let choice_left node_idx prf =
  let seq = Proof.get_seq node_idx prf in
  match (Seq.find_index_left Form.is_choice seq) with
  | None ->
    Rule.fail node_idx prf
  | Some seq_idx ->
    choice_left_at seq_idx node_idx prf

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

(* Applies the left rule for the left-most antecdent star formula *)
let star_left node_idx prf =
  let seq = Proof.get_seq node_idx prf in
  match (Seq.find_index_left Form.is_star seq) with
  | None ->
    Rule.fail node_idx prf
  | Some seq_idx ->
    star_left_at seq_idx node_idx prf

(* Requires: [idx] is the index of a formula in the consequent.
             For efficiency, we don't check that here, since this should only
             be called by other tactics that ensure this is the case. *)
let star_right_at idx =
  let rl seq =
    let e_star = Seq.nth seq idx in
    match (Form.dest_star e_star) with
    | None ->
      []
    | Some e ->
      let premise =
        Seq.insert_many
          [Form.one; Form.concat e e_star ]
          idx
          (Seq.remove_at idx seq) in
      [
        [ (premise, Tagpairs.mk (Seq.tags seq), Tagpairs.empty) ],
            Infrule.star_right
      ] in
  Rule.mk_infrule rl

let star_right node_idx prf =
  let seq = Proof.get_seq node_idx prf in
  match (Seq.find_index_right Form.is_star seq) with
  | None ->
    []
  | Some idx ->
    star_right_at idx node_idx prf

(* Concatenation Right Rules *)

(* This invertible rule peels off the matching first letter from the antecedent
   and all consequent formulas. *)
let concat_right_first_letter =
  let rl seq =
    match (Seq.consequent seq) with
    | [] ->
      []
    | fs ->
      let fs' = List.map Form.factorise fs in
      let (first, rest) =
        List.split (List.map (fun fs -> List.hd fs, List.tl fs) fs') in
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

let split_right_singleton f =
  List.map
    (Pair.map (fun fs -> [Form.concatenate fs]))
    (Blist.all_splits ~include_empty:false (Form.factorise f))

(* This version splits along all common prefixes and suffixes, not simply the
   maximal ones. *)
let split_right_multiple_all fs =
  let fss = List.map Form.factorise fs in
  let common_pre = Blist.longest_common_prefix ~eq:Form.equal fss in
  let front_splits =
    if (Int.equal (List.length common_pre) 0) then
      []
    else
      List.mapi
        (fun i _ ->
          let left = [Form.concatenate (List.take (i+1) common_pre)] in
          let right =
            List.map (fun fs -> Form.concatenate (List.drop (i+1) fs)) fss in
          (left, right))
        (common_pre) in
  let common_suf = Blist.longest_common_suffix ~eq:Form.equal fss in
  if (Int.equal (List.length common_suf) 0) then
    List.rev front_splits
  else
    let rev_fss = List.map List.rev fss in
    let rev_suf = List.rev common_suf in
    let back_splits =
      List.mapi
        (fun i _ ->
          let left =
            List.map
              (fun fs -> Form.concatenate (List.rev (List.drop (i+1) fs)))
              (rev_fss) in
          let right = [Form.concatenate (List.rev (List.take (i+1) rev_suf))] in
          (left, right))
        (common_suf) in
    Blist.interleave (List.rev front_splits) (List.rev back_splits)

(* This version splits along only the longest common prefix and suffix.
   In the proof search strategy, all splits are tried eventually as this is
   applied repeatedly by the [right_decomposition] tactic, below. *)
let split_right_multiple_maximal fs =
  let fss = List.map Form.factorise fs in
  let common_pre = Blist.longest_common_prefix ~eq:Form.equal fss in
  let front_split =
    let prefix_len = List.length common_pre in
    Option.mk_lazily
      (not (Int.equal prefix_len 0))
      (fun () ->
        let left = [Form.concatenate common_pre] in
        let right =
          List.map (fun fs -> Form.concatenate (List.drop prefix_len fs)) fss in
        (left, right)) in
  let common_suf = Blist.longest_common_suffix ~eq:Form.equal fss in
    let back_split =
      let suffix_len = List.length common_suf in
      Option.mk_lazily
        (not (Int.equal suffix_len 0))
        (fun () ->
          let left =
            List.map
              (fun fs ->
                Form.concatenate (List.take ((List.length fs) - suffix_len) fs))
              (fss) in
          let right = [Form.concatenate common_suf] in
          (left, right)) in
    Option.list_get [front_split; back_split]

let concat_right ?(non_maximal=false) =
  let rl seq =
    let consequent_splits =
      match (Seq.consequent seq) with
      | [] ->
        []
      | [ f ] ->
        split_right_singleton f
      | fs ->
        if non_maximal then
          split_right_multiple_all fs
        else
          split_right_multiple_maximal fs in
    if (List.is_empty consequent_splits) then
      []
    else
      let antecedent_splits =
        (Seq.all_left_splits (Seq.with_consequent [] seq)) in
      let antecedent_splits =
        List.map
          (Pair.map (fun seq -> (seq, Tagpairs.mk (Seq.tags seq))))
          (antecedent_splits) in
      Blist.cartesian_map
        (fun ((gamma, gtps), (delta, dtps)) (sigma, pi) ->
          [ (Seq.with_consequent sigma gamma, gtps, Tagpairs.empty);
            (Seq.with_consequent pi delta, dtps, Tagpairs.empty) ],
              Infrule.concat_right)
        (antecedent_splits)
        (consequent_splits) in
  Rule.mk_infrule rl

(* Weakening Tactics *)

(* Weaken away all consequent formulas that do not match the (single)
   antecendent formula. This is designed to be pre-composed with the [axiom]
   rule. *)
let wk_for_axiom =
  let rl seq =
    match (Seq.antecedent seq, Seq.consequent seq) with
    | ([e], [f]) when Form.equal e f ->
      (* Fail if the sequent is already an instance of the axiom *)
      []
    | ([e], fs) when List.exists (Form.equal e) fs ->
      let premise = Seq.with_consequent [e] seq in
      [
        [ (premise, Tagpairs.mk (Seq.tags premise), Tagpairs.empty) ],
            Infrule.weaken
      ]
    | (_, _) ->
      [] in
  Rule.mk_infrule rl

(* Weaken away all consequent formulas except a single occurrence of [Form.one].
   This rule fails if there are no occurrences of [Form.one] in the consequent,
   or if the consequent is already the singleton list containint [Form.one].
   This is designed to be pre-composed with the [one_right] axiom. *)
let wk_leave_one =
  let rl seq =
    match (Seq.consequent seq) with
    | _::_ when Seq.exists_right Form.is_one seq ->
      let premise = Seq.filter_right Form.is_one seq in
      let premise = Seq.take_right 1 premise in
      [
        [ (premise, Tagpairs.mk (Seq.tags premise), Tagpairs.empty) ],
            Infrule.weaken
      ]
    | _  ->
      [] in
  Rule.mk_infrule rl

(* Weaken away all consequent formulas. This is designed to be pre-composed with
   the [zero_left] axiom. *)
let wk_all =
  let rl seq =
    match (Seq.consequent seq) with
    | [] ->
      []
    | _ ->
      let premise = Seq.with_consequent [] seq in
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

(* Weaken the formula at the given position.
   Requires: the given index to be a valid index in the consequent.
             For efficiency, we don't check that here, since this should only
             be called by other tactics that ensure this is the case.*)
let wk_at idx =
  let rl seq =
    let premise = Seq.remove_at idx seq in
    [
      [ (premise, Tagpairs.mk (Seq.tags seq), Tagpairs.empty) ],
          Infrule.weaken
    ]
  in
  Rule.mk_infrule rl

(* Produce all non-empty combinations of weakenings *)
let wk_combs =
  let rl seq =
    match Seq.consequent seq with
    | [] | [_] ->
      []
    | consequent ->
      let tps = Tagpairs.mk (Seq.tags seq) in
      List.map
        (fun consequent ->
          ([(Seq.with_consequent consequent seq), tps, Tagpairs.empty], Infrule.weaken))
        (Blist.all_combs consequent)
  in
  Rule.mk_infrule rl

(* Cut *)

let cut_wrt seqs =
  let cut_wrt seq cut_seq =
    let cut_antecedent = Seq.antecedent cut_seq in
    let len_cut_antecedent = List.length cut_antecedent in
    match Blist.sublist_last_index (cut_antecedent) (Seq.antecedent seq) with
    | Some idx when len_cut_antecedent > 0
                 && (idx > 0 || len_cut_antecedent < pred (Seq.right_start seq))
        ->
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
    else if (Tags.is_empty (Seq.tags bud)) then
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
    let from_weakening idx =
      match (Node.dest (Proof.find idx prf)) with
      | (_, Some Infrule.Weaken) ->
        true
      | _ ->
        false in
    (* I thought we should not allow cycles to cross weakening rules, but this
       is OK for invertible weakening rules, I think. *)
    (* let oldest_to_youngest = Rule.ancestor_nodes idx prf in
    let youngest_to_oldest = List.rev (oldest_to_youngest) in
    match (List.find_index from_weakening youngest_to_oldest) with
    | None ->
      oldest_to_youngest
    | Some i ->
      List.rev (List.take i youngest_to_oldest) in *)
    List.filter (Fun.neg from_weakening) (Rule.ancestor_nodes idx prf) in
  (* Sort the potential targets by consequent size, then antecendent size.
     We use a stable sort, so older nodes come first. *)
  let targets =
    List.stable_sort
      (fun idx idx' ->
        let seq = Proof.get_seq idx prf in
        let seq' = Proof.get_seq idx' prf in
        let result =
          List.compare_lengths (Seq.consequent seq) (Seq.consequent seq') in
        if Int.equal result 0
          then (Seq.right_start seq) - (Seq.right_start seq')
          else result)
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

(* Apply the star right rule, immediately followed by a weakening that removes
   the empty case of the resulting disjunction. *)
let star_right_non_empty node_idx prf =
  let seq = Proof.get_seq node_idx prf in
  match (Seq.find_index_right Form.is_star seq) with
  | None ->
    Rule.fail node_idx prf
  | Some idx ->
    (Rule.compose (star_right_at idx) (wk_at idx)) node_idx prf

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
  Rule.compose
    (Rule.non_empty (Rule.repeat rl_if_not_axiom))
    (Rule.attempt one_left)
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
    Rule.non_empty (Rule.repeat right_rule) ;
    Rule.non_empty (Rule.repeat weakenings) ;
  ]

let search_step =
  Rule.non_empty
    (Rule.sequence [
      Rule.attempt left_phase ;
      Rule.attempt right_invertible_phase ;
      Rule.first [
          (* No-op if we can close using an axiom in the next step *)
          Rule.conditional
            (Seq.is_axiomatic ~nonatomic:(not !atomic_axioms))
            (Rule.identity) ;
          Rule.attempt backlink ;
        ] ;
    ])

let wk_non_invertible =
  Rule.conditional
    (fun seq ->
      match (Seq.antecedent seq) with
      | e::_ when Form.is_letter e -> true
      | _ -> false)
    (Rule.compose
      (Rule.attempt wk_non_matching)
      (wk_combs))

let left_decomposition =
  Rule.repeat
    (Rule.first [
      (* Go no further if we can apply zero-left rule *)
      Rule.conditional (Seq.exists_left Form.is_zero) (Rule.fail) ;
      (* Otherwise, continue *)
      one_left ;
      concat_left ;
      choice_left ;
    ])

let right_decomposition idx prf =
  let is_new_subgoal idx' prf' =
    let seq = Proof.get_seq idx' prf' in
    let ancestry = Proof.get_ancestry_since idx idx' prf' in
    let res =
      List.for_all
        (fun (idx'', node) -> not (Seq.equal seq (Node.get_seq node)))
        (ancestry) in
    res
  in
  let is_axiomatic idx prf =
    let seq = Proof.get_seq idx prf in
    Seq.is_axiomatic ~nonatomic:(not !atomic_axioms) seq in
  Rule.repeat'
    ~while_:is_new_subgoal
    ~until:is_axiomatic
    (Rule.first [
        wk_duplicates ;
        wk_non_matching ;
        choice_right ;
        Rule.compose_pairwise
          concat_right_first_letter
          [Rule.identity; Rule.attempt backlink];
        Rule.choice [ star_right ; star_right_non_empty ] ;
        Rule.compose concat_right (Rule.attempt backlink) ;
      ])
    (idx)
    (prf)

(* Proof-search strategies *)

let axioms =
  ref
    (Rule.first [
      Rule.compose (Rule.attempt wk_for_axiom) axiom ;
      Rule.compose (Rule.attempt wk_all) zero_left ;
      Rule.compose (Rule.attempt wk_leave_one) one_right ;
    ])

(* Keep the old search strategy, for reference *)
let rules =
  ref
    (Rule.first [
      backlink ;
      Rule.choice [
          wk_non_invertible ;
          search_step;
          cut_backlink;
          concat_right ~non_maximal:true;
        ] ;
    ])

let rules =
  ref
    (Rule.first [
      backlink ;
      Rule.choice [
        wk_non_invertible ;
        cut_backlink ;
        Rule.non_empty (Rule.compose left_decomposition right_decomposition) ;
        Rule.compose left_decomposition star_left ;
      ]
    ])

(* Inline tests *)
module _ = struct
  let print_splits fmt zs =
    Format.pp_print_list ~pp_sep:Format.pp_force_newline
      (fun fmt (xs, ys) ->
        Format.fprintf fmt "(%a; %a)"
          (Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
            Form.pp) xs
          (Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
            Form.pp) ys)
      (fmt)
      (zs)

  let a = Form.letter 'a'
  let b = Form.letter 'b'
  let c = Form.letter 'c'
  let d = Form.letter 'd'
  let e = Form.letter 'e'
  let f = Form.letter 'f'

  let%expect_test _ =
    let form1 = Form.concatenate [a;b;c;e;f] in
    let form2 = Form.concatenate [a;b;d;e;f] in
    let result = split_right_multiple_all [form1;form2] in
    print_endline (Format.asprintf "%a" print_splits result) ;
    [%expect{|
      (ab; cef, def)
      (abc, abd; ef)
      (a; bcef, bdef)
      (abce, abde; f)
    |}]

  let%expect_test _ =
    let form1 = Form.concatenate [a;b;c;e;f] in
    let form2 = Form.concatenate [a;b;d;e;f] in
    let result = split_right_multiple_maximal [form1;form2] in
    print_endline (Format.asprintf "%a" print_splits result) ;
    [%expect{|
      (ab; cef, def)
      (abc, abd; ef)
    |}]

end