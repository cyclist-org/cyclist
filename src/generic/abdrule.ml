open Lib

(* using L should allow switching between Blist and Zlist easily *)
module L = Blist

module Make(Seq : Sigs.SEQUENT)(Defs : Sigs.DEFS) =
struct
  module Proof = Proof.Make(Seq)
  module Rule = Proofrule.Make(Seq)

  type seq_t = Seq.t
  type proof_t = Proof.t
  type defs_t = Defs.t
  type rule_t = Rule.t

  type select_f = int -> proof_t -> int list
  type infrule_app = (seq_t * Tagpairs.t * Tagpairs.t) list * string

  type abdinfrule_f = seq_t -> defs_t -> defs_t list
  type abdbackrule_f = seq_t -> seq_t -> defs_t -> defs_t list
  type abdgenrule_f = seq_t -> defs_t -> (infrule_app * defs_t) list

  type t =
    int -> proof_t -> defs_t -> ((int list * proof_t) * defs_t) L.t

  let mk_abdinfrule r idx prf defs =
    let seq = Proof.get_seq idx prf in
    let apps = r seq defs in
    L.map
      (fun newdefs -> (([idx], prf),newdefs))
      (L.of_list apps)


  let mk_abdbackrule sel_f abr_f srcidx prf defs =
    let srcseq = Proof.get_seq srcidx prf in
    let trgidxs = L.of_list (sel_f srcidx prf) in
    let apply trgidx =
      let trgseq = Proof.get_seq trgidx prf in
      L.map
        (fun defs' -> (([srcidx], prf), defs'))
        (L.of_list (abr_f srcseq trgseq defs)) in
    L.bind apply trgidxs

  let mk_abdgenrule r idx prf defs =
    let seq = Proof.get_seq idx prf in
    let mk ((l,d),defs') = (Proof.add_inf idx d l prf,defs') in
    L.map mk (L.of_list (r seq defs))

  let fail _ _ _ = L.empty

  let lift r idx prf defs = L.map (fun p -> (p,defs)) (r idx prf)

  let apply_to_subgoals r ((subgoals, prf), defs) =
    Blist.fold_left
      (* close one subgoal each time by actually appling the rule *)
      (fun apps idx ->
        L.bind
          (fun ((opened, oldprf), olddefs) ->
            (* add new subgoals to the list of opened ones *)
            L.map
              (fun ((newsubgoals, newprf), newdefs) ->
                ((opened @ newsubgoals, newprf), newdefs))
              (r idx oldprf olddefs))
          apps)
        (L.singleton (([], prf), defs))
      subgoals

  let compose r r' idx prf defs =
    L.bind (apply_to_subgoals r') (r idx prf defs)

  let choice rl idx prf defs =
    L.bind (fun f -> f idx prf defs) (L.of_list rl)

  let rec first rl idx prf defs =
    match rl with
    | [] -> L.empty
    | r::rs ->
      let apps = r idx prf defs in
      if not (L.is_empty apps) then apps else
      first rs idx prf defs

  let attempt r idx prf defs =
    let apps = r idx prf defs in
    if not (L.is_empty apps) then apps else
    L.singleton (([idx],prf), defs)

end
