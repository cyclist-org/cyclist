open Lib

(* using L should allow switching between Blist and Zlist easily *)
module L = Blist

module type S = sig
  type seq_t

  type proof_t

  type axiom_f = seq_t -> string option

  type infrule_app = (seq_t * Tagpairs.t * Tagpairs.t) list * string

  type infrule_f = seq_t -> infrule_app list

  type backrule_f = seq_t -> seq_t -> (Tagpairs.t * string) list

  type select_f = int -> proof_t -> int list

  type t = int -> proof_t -> (int list * proof_t) L.t

  val mk_axiom : axiom_f -> t

  val mk_infrule : infrule_f -> t

  val mk_backrule : bool -> select_f -> backrule_f -> t

  val all_nodes : select_f

  val closed_nodes : select_f

  val ancestor_nodes : select_f

  val syntactically_equal_nodes : select_f

  val default_select_f : select_f ref

  val set_default_select_f : int -> unit

  val default_select_f_descr : ?line_prefix:string -> unit -> string

  val fail : t

  val identity : t

  val attempt : t -> t

  val compose : t -> t -> t

  val compose_pairwise : t -> t list -> t

  val choice : t list -> t

  val first : t list -> t

  val sequence : t list -> t

  val conditional : (seq_t -> bool) -> t -> t

  val combine_axioms : t -> t -> t
end

module Make (Seq : Sequent.S) = struct
  module Proof = Proof.Make (Seq)
  module Node = Proofnode.Make (Seq)

  type seq_t = Seq.t

  type proof_t = Proof.t

  type axiom_f = seq_t -> string option

  type infrule_app = (seq_t * Tagpairs.t * Tagpairs.t) list * string

  type infrule_f = seq_t -> infrule_app list

  type t = int -> Proof.t -> (int list * Proof.t) L.t

  type backrule_f = seq_t -> seq_t -> (Tagpairs.t * string) list

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
      debug (fun () -> "Found " ^ d ^ " app.") ;
      Proof.add_inf idx d l prf
    in
    L.map mk (L.of_list (r_f seq))

  let mk_backrule greedy sel_f br_f srcidx prf =
    let srcseq = Proof.get_seq srcidx prf in
    let trgidxs = L.of_list (sel_f srcidx prf) in
    let mk trgidx (vtts, d) =
      ([], Proof.add_backlink srcidx d trgidx vtts prf)
    in
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

  let syntactically_equal_nodes srcidx prf =
    let seq = Proof.get_seq srcidx prf in
    let nodes =
      L.filter
        (fun (idx, n) ->
          (* TODO: Surely this should be equal_upto_tags? *)
          Seq.equal seq (Node.get_seq n) && not (Int.equal idx srcidx) )
        (Proof.to_list prf)
    in
    L.map fst nodes

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
      | 3 ->
        syntactically_equal_nodes
      | _ ->
        !default_select_f

  let default_select_f_descr ?(line_prefix = "\t") () =
    line_prefix ^ "0 -- all proof nodes (DEFAULT)\n" ^
    line_prefix ^ "1 -- all closed proof nodes\n" ^
    line_prefix ^ "2 -- all ancestor proof nodes\n" ^
    line_prefix ^ "3 -- all syntactically equal proof nodes"

  let fail _ _ = L.empty

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
    try
      L.fold_left2
        (* close one subgoal each time by actually appling the corresponding rule *)
          (fun apps r idx ->
          L.bind
            (fun (opened, oldprf) ->
              (* add new subgoals to the list of opened ones *)
              L.map
                (fun (newsubgoals, newprf) -> (opened @ newsubgoals, newprf))
                (r idx oldprf) )
            apps )
        (L.singleton ([], prf))
        rules subgoals
    with Invalid_argument _ -> L.empty

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

  let identity idx prf = L.singleton ([idx], prf)

  let attempt r idx prf =
    let apps = r idx prf in
    if not (L.is_empty apps) then apps else identity idx prf

  let rec sequence = function
    | [] -> identity
    | r :: rs -> compose r (sequence rs)

  let conditional cond r idx prf =
    if cond (Proof.get_seq idx prf) then r idx prf else []

  let combine_axioms ax rl = first [ax; compose rl (attempt ax)]
end
