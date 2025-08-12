open Lib

(* using L should allow switching between Blist and Zlist easily *)
module L = Blist

module type S = sig

  type seq_t
  type infrule_t
  type proof_t
  type axiom_f = seq_t -> infrule_t option
  type infrule_f =
    seq_t -> ((seq_t * Tagpairs.t * Tagpairs.t) list * infrule_t) list
  type backrule_f = seq_t -> seq_t -> Tagpairs.t list
  type select_f = int -> proof_t -> int list
  type t = int -> proof_t -> (int list * proof_t) L.t

  val mk_axiom : axiom_f -> t
  val mk_infrule : infrule_f -> t
  val mk_backrule : bool -> select_f -> backrule_f -> t
  val all_nodes : select_f
  val closed_nodes : select_f
  val ancestor_nodes : select_f
  val default_select_f : select_f ref
  val set_default_select_f : int -> unit
  val default_select_f_descr : ?line_prefix:string -> unit -> string
  val fail : t
  val identity : t
  val attempt : t -> t
  val non_empty : t -> t
  val compose : t -> t -> t
  val compose_pairwise : t -> t list -> t
  val choice : t list -> t
  val first : t list -> t
  val sequence : t list -> t
  val conditional : (seq_t -> bool) -> t -> t
  val unless : (seq_t -> bool) -> t -> t
  val repeat' :
    ?while_:(int -> proof_t -> bool) -> ?until:(int -> proof_t -> bool)
        -> ?failure_as_termination:bool -> ?eager:bool -> t -> t
  val repeat : t -> t
end

module Make (Seq : Sequent.S) (Infrule : Infrule.S) = struct

  module Proof = Proof.Make (Seq) (Infrule)
  module Node = Proofnode.Make (Seq) (Infrule)

  type axiom_f = Seq.t -> Infrule.t option

  type infrule_f =
    Seq.t -> ((Seq.t * Tagpairs.t * Tagpairs.t) list * Infrule.t) list

  type t = int -> Proof.t -> (int list * Proof.t) L.t

  type backrule_f = Seq.t -> Seq.t -> Tagpairs.t list

  type select_f = int -> Proof.t -> int list

  (* Apply the sequent in the open node identified by idx in prf to the
	   characterising function ax_f.
     If we get back Some descr then the sequent is the conclusion of the axiom
     characterised by ax_f (the name of the axiom is given by descr), so return
     a singleton list containing the original proof prf updated by closing the
     open node using the axiom descr, which does not add any new open nodes,
     since axioms do not have any premises.
     Otherwise return an empty list of results
	*)
  let mk_axiom ax_f idx prf =
    match ax_f (Proof.get_seq idx prf) with
    | None -> L.empty
    | Some descr -> L.singleton ([], Proof.add_axiom idx descr prf)

  let mk_infrule r_f idx prf =
    let seq = Proof.get_seq idx prf in
    let mk (l, d) =
      debug (fun () -> Format.asprintf "Found %a app." Infrule.pp d) ;
      Proof.add_inf idx d l prf
    in
    L.map mk (L.of_list (r_f seq))

  let mk_backrule greedy sel_f br_f srcidx prf =
    let srcseq = Proof.get_seq srcidx prf in
    let trgidxs = L.of_list (sel_f srcidx prf) in
    let mk trgidx vtts =
      ([], Proof.add_backlink srcidx trgidx vtts prf) in
    let check (_, p) = Proof.check p in
    let apply trgidx =
      let trgseq = Proof.get_seq trgidx prf in
      L.map (mk trgidx) (L.of_list (br_f srcseq trgseq))
    in
    let apps = L.bind apply trgidxs in
    if greedy then Option.dest L.empty L.singleton (L.find_opt check apps)
    else L.filter check apps

  let all_nodes srcidx prf =
    L.filter
      (fun idx -> not (Int.equal idx srcidx))
      (L.map fst (Proof.to_list prf))

  let closed_nodes srcidx prf =
    let nodes =
      L.filter
        (fun (idx, n) -> (not (Node.is_open n)) && not (Int.equal idx srcidx))
        (Proof.to_list prf)
    in
    L.map fst nodes

  let ancestor_nodes srcidx prf = L.map fst (Proof.get_ancestry srcidx prf)

  let default_select_f = ref all_nodes

  let set_default_select_f id =
    default_select_f :=
      match id with
      | 0 ->
        all_nodes
      | 1 ->
        closed_nodes
      | 2 ->
        ancestor_nodes
      | _ ->
        !default_select_f

  let default_select_f_descr ?(line_prefix = "\t") () =
    line_prefix ^ "0 -- all proof nodes (DEFAULT)\n" ^
    line_prefix ^ "1 -- all closed proof nodes\n" ^
    line_prefix ^ "2 -- all ancestor proof nodes\n"

  let fail _ _ = L.empty

  let identity idx prf = L.singleton ([idx], prf)

  let non_empty r idx prf =
    match r idx prf with
    | [([idx'], _)] when idx' = idx ->
      []
    | result ->
      result

  (* This has been generalised below to take a list of rules                   *)
  (*                                                                           *)
  (* let apply_to_subgoals r (subgoals, prf) =                                 *)
  (*   L.fold_left                                                             *)
  (*     (* close one subgoal each time by actually appling the rule *)        *)
  (*     (fun apps idx ->                                                      *)
  (*       L.bind                                                              *)
  (*         (fun (opened, oldprf) ->                                          *)
  (*           (* add new subgoals to the list of opened ones *)               *)
  (*           L.map                                                           *)
  (*             (fun (newsubgoals, newprf) -> (opened @ newsubgoals, newprf)) *)
  (*             (r idx oldprf))                                               *)
  (*         apps)                                                             *)
  (*     (L.singleton ([], prf))                                               *)
  (*     subgoals                                                              *)

  let apply_to_subgoals_pairwise rules (subgoals, prf) =
    (* First make sure we have one rule for each subgoal,
       truncating or padding with identity as necessary *)
    let rules =
      let num_rules = L.length rules in
      let num_subgoals = L.length subgoals in
      if (num_rules < num_subgoals) then
        L.append rules (L.repeat identity (num_subgoals - num_rules))
      else
        L.take num_subgoals rules in
    (* close one subgoal each time by actually applying corresponding rule *)
    L.fold_left2
      (fun apps r idx ->
        L.bind
          (fun (opened, oldprf) ->
            (* add new subgoals to the list of opened ones *)
            L.map
              (fun (newsubgoals, newprf) -> (opened @ newsubgoals, newprf))
              (r idx oldprf) )
          apps)
      (L.singleton ([], prf))
      rules subgoals

  let compose r r' idx prf =
    L.bind
      (fun ((subgoals, _) as res) ->
        apply_to_subgoals_pairwise
          (L.repeat r' (L.length subgoals))
          res)
      (r idx prf)

  let compose_pairwise r rs idx prf =
    L.bind (apply_to_subgoals_pairwise rs) (r idx prf)

  let choice rl idx prf = L.bind (fun f -> f idx prf) (L.of_list rl)

  let rec first rl idx prf =
    match rl with
    | [] -> L.empty
    | r :: rs ->
        let apps = r idx prf in
        if not (L.is_empty apps) then apps else first rs idx prf

  let attempt r idx prf =
    let apps = r idx prf in
    if not (L.is_empty apps) then apps else identity idx prf

  let rec sequence = function
    | [] -> identity
    | r :: rs -> compose r (sequence rs)

  let repeat'
        ?(while_=(fun _ _ -> true))
        ?(until=(fun _ _ -> false))
        ?(failure_as_termination=true)
        ?(eager=false)
      r =
    let rec repeat idx prf =
      if (not (while_ idx prf)) then
        fail idx prf
      else if (until idx prf) then
        identity idx prf
      else
        let apps = r idx prf in
        if (L.is_empty apps) then
          (if failure_as_termination then identity idx prf else fail idx prf)
        else if eager then
          let res =
            L.find_map_or
              (fun ((subgoals, _) as res) ->
                apply_to_subgoals_pairwise
                  (L.repeat repeat (L.length subgoals))
                  res)
              (List.exists (fun (subgoals, _) -> L.is_empty subgoals))
              apps in
          match res with
          | Left res ->
            [ L.find (fun (subgoals, _) -> L.is_empty subgoals) res ]
          | Right res ->
            L.flatten res
        else
          L.bind
            (fun ((subgoals, _) as res) ->
              apply_to_subgoals_pairwise
                (L.repeat repeat (L.length subgoals))
                res)
            apps in
    repeat

  (* Note that (repeat r) is functionally equivalent to (repeat' r), but
     implementing it afresh avoids the conditional checks on each recursive
     call. So, the duplication of code here is justified in the name of being
     slightly more efficient. *)
  let repeat r =
    let rec repeat idx prf =
      let apps = r idx prf in
      if (L.is_empty apps) then
        identity idx prf
      else
        L.bind
          (fun ((subgoals, _) as res) ->
            apply_to_subgoals_pairwise
              (L.repeat repeat (L.length subgoals))
              res)
          apps in
    repeat

  let conditional cond r idx prf =
    if cond (Proof.get_seq idx prf) then r idx prf else []

  let unless cond r =
    conditional (fun s -> not (cond s)) r

end
