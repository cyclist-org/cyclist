open Lib

module type S = sig

  type seq_t

  type infrule_t

  type proof_t

  type defs_t

  type rule_t

  type select_f = int -> proof_t -> int list

  type infrule_app = (seq_t * Tagpairs.t * Tagpairs.t) list * infrule_t

  type abdinfrule_f = seq_t -> defs_t -> defs_t list

  type abdbackrule_f = seq_t -> seq_t -> defs_t -> defs_t list

  type abdgenrule_f = seq_t -> defs_t -> (infrule_app * defs_t) list

  type t = int -> proof_t -> defs_t -> ((int list * proof_t) * defs_t) list

  val mk_abdinfrule : abdinfrule_f -> t

  val mk_abdbackrule : select_f -> abdbackrule_f -> t

  val mk_abdgenrule : abdgenrule_f -> t

  val fail : t

  val lift : rule_t -> t

  val compose : t -> t -> t

  val choice : t list -> t

  val attempt : t -> t

  val first : t list -> t
end

module Make (Seq : Sequent.S) (Infrule : Infrule.S) (Defs : sig type t end) =
struct

  module Proof = Proof.Make (Seq)(Infrule)
  module Rule = Proofrule.Make (Seq)(Infrule)

  type select_f = int -> Proof.t -> int list

  type infrule_app = (Seq.t * Tagpairs.t * Tagpairs.t) list * Infrule.t

  type abdinfrule_f = Seq.t -> Defs.t -> Defs.t list

  type abdbackrule_f = Seq.t -> Seq.t -> Defs.t -> Defs.t list

  type abdgenrule_f = Seq.t -> Defs.t -> (infrule_app * Defs.t) list

  type t = int -> Proof.t -> Defs.t -> ((int list * Proof.t) * Defs.t) list

  let mk_abdinfrule r idx prf defs =
    let seq = Proof.get_seq idx prf in
    let apps = r seq defs in
    List.map (fun newdefs -> (([idx], prf), newdefs)) apps

  let mk_abdbackrule sel_f abr_f srcidx prf defs =
    let srcseq = Proof.get_seq srcidx prf in
    let trgidxs = sel_f srcidx prf in
    let apply trgidx =
      let trgseq = Proof.get_seq trgidx prf in
      List.map
        (fun defs' -> (([srcidx], prf), defs'))
        (abr_f srcseq trgseq defs)
    in
    List.concat_map apply trgidxs

  let mk_abdgenrule r idx prf defs =
    let seq = Proof.get_seq idx prf in
    let mk ((l, d), defs') = (Proof.add_inf idx d l prf, defs') in
    List.map mk (r seq defs)

  let fail _ _ _ = []

  let lift r idx prf defs = List.map (fun p -> (p, defs)) (r idx prf)

  let apply_to_subgoals r ((subgoals, prf), defs) =
    List.fold_left
      (* close one subgoal each time by actually appling the rule *)
        (fun apps idx ->
        List.concat_map
          (fun ((opened, oldprf), olddefs) ->
            (* add new subgoals to the list of opened ones *)
            List.map
              (fun ((newsubgoals, newprf), newdefs) ->
                ((opened @ newsubgoals, newprf), newdefs) )
              (r idx oldprf olddefs) )
          apps )
      [(([], prf), defs)]
      subgoals

  let compose r r' idx prf defs =
    List.concat_map (apply_to_subgoals r') (r idx prf defs)

  let choice rl idx prf defs = List.concat_map (fun f -> f idx prf defs) rl

  let rec first rl idx prf defs =
    match rl with
    | [] -> []
    | r :: rs ->
        let apps = r idx prf defs in
        if not (List.is_empty apps) then apps else first rs idx prf defs

  let attempt r idx prf defs =
    let apps = r idx prf defs in
    if not (List.is_empty apps) then apps else [(([idx], prf), defs)]
end
