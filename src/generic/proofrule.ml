open Util

(* using L should allow switching between Blist and Zlist easily *)
module L = Zlist

module Make(Seq : Sigs.SEQUENT) =
struct
  module Proof = Proof.Make(Seq)
  
  type seq_t = Seq.t
  type proof_t = Proof.t
  type axiom_f = seq_t -> string option
  type infrule_app = (seq_t * Util.TagPairs.t * Util.TagPairs.t) list * string
  type infrule_f = seq_t -> infrule_app list
  type t = int -> Proof.t -> (int list * Proof.t) L.t
  
  let mk_axiom ax_f idx prf =
    match ax_f (Proof.get_seq idx prf) with
    | None -> L.empty
    | Some descr -> L.cons ([], Proof.add_axiom idx descr prf) L.empty  

  
  let mk_infrule r_f idx prf =
    let seq = Proof.get_seq idx prf in
    let mk (l,d) = Proof.add_inf idx d l prf in
    L.map mk (L.of_list (r_f seq))

  type backrule_f = seq_t -> seq_t -> (Util.TagPairs.t * string) list
  type select_f = int -> Proof.t -> int list
        
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
      
  let rec repeat n r = 
    if n<=0 then invalid_arg "repeat" else
    if n=1 then r else
    compose r (repeat (n-1) r)  
end