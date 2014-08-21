open Lib
open Util

(* using L should allow switching between Blist and Zlist easily *)
module L = Blist

module Make(Seq : Sigs.SEQUENT) =
struct
  module Proof = Proof.Make(Seq)
  
  type seq_t = Seq.t
  type proof_t = Proof.t
  type axiom_f = seq_t -> string option
  type infrule_app = (seq_t * Util.TagPairs.t * Util.TagPairs.t) list * string
  type infrule_f = seq_t -> infrule_app list
  type t = int -> Proof.t -> (int list * Proof.t) L.t
  type backrule_f = seq_t -> seq_t -> (Util.TagPairs.t * string) list
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
    let mk (l,d) = 
      debug (fun () -> "Found " ^ d ^ " app.") ;
      Proof.add_inf idx d l prf in
    L.map mk (L.of_list (r_f seq))

  let mk_backrule greedy sel_f br_f srcidx prf =
    let srcseq = Proof.get_seq srcidx prf in
    let trgidxs = L.of_list (sel_f srcidx prf) in
    let mk trgidx (vtts,d) = 
      ([], Proof.add_backlink srcidx d trgidx vtts prf) in
    let check (_,p) = Proof.check p in
    let apply trgidx = 
      let trgseq = Proof.get_seq trgidx prf in
      L.map (mk trgidx) (L.of_list (br_f srcseq trgseq)) in
    let apps = L.bind apply trgidxs in
    if greedy then
      Option.dest L.empty L.singleton (L.find_first check apps) 
    else  
      L.filter check apps
    
  
  let all_nodes srcidx prf = 
    Blist.filter (fun idx -> idx<>srcidx) (Blist.map fst (Proof.to_list prf))
  let ancestor_nodes srcidx prf = Blist.map fst (Proof.get_ancestry srcidx prf)

  
  let fail _ _ = L.empty

  let apply_to_subgoals r (subgoals, prf) =
    Blist.fold_left
      (* close one subgoal each time by actually appling the rule *)
      (fun apps idx ->
        L.bind
          (fun (opened, oldprf) ->
            (* add new subgoals to the list of opened ones *)
            L.map
              (fun (newsubgoals, newprf) -> (opened @ newsubgoals, newprf))
              (r idx oldprf))
          apps)
        (L.singleton ([], prf)) 
      subgoals
    
  let compose r r' idx prf = L.bind (apply_to_subgoals r') (r idx prf)

  let choice rl idx prf = L.bind (fun f -> f idx prf) (L.of_list rl) 
  
  let rec first rl idx prf = 
    match rl with
    | [] -> L.empty
    | r::rs -> 
      let apps = r idx prf in
      if not (L.is_empty apps) then apps else first rs idx prf 

  let identity idx prf = 
    L.singleton ([idx],prf)
      
  let attempt r idx prf = 
    let apps = r idx prf in
    if not (L.is_empty apps) then apps else
    identity idx prf

  let rec sequence = function
    | [] -> identity
    | r::rs -> compose r (sequence rs)
  
  let conditional cond r idx prf = 
    if cond (Proof.get_seq idx prf) then r idx prf else []
  
end