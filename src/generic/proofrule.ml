open Util

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
    if greedy then
      begin
        let rsl = L.find_first check (L.flatten (L.map apply trgidxs)) in
        match rsl with
        | None -> L.empty
        | Some rsl' -> L.cons rsl' L.empty
      end
    else  
      L.filter check (L.flatten (L.map apply trgidxs))
    
  
  let all_nodes srcidx prf = 
    Blist.filter (fun idx -> idx<>srcidx) (Blist.map fst (Proof.to_list prf))
  let ancestor_nodes srcidx prf = Blist.map fst (Proof.get_ancestry srcidx prf)

  let fail _ _ = L.empty

  let apply_to_subgoals r (subgoals, prf) =
    L.of_list (
    Blist.fold_left
      (* close one subgoal each time *)
      (fun apps idx ->
        Blist.flatten
          (* actually apply the rule *)
          (Blist.map
            (fun (opened, oldprf) ->
              (* add new subgoals to the list of opened ones *)
              Blist.map
                (fun (newsubgoals, newprf) -> (opened @ newsubgoals, newprf))
                (L.to_list (r idx oldprf)))
          apps))
      [ ([], prf) ]
      subgoals
    )
    
  let compose (r:t) r' idx prf =
    L.flatten (L.map (apply_to_subgoals r') (r idx prf))

  let choice rl idx prf =
    L.flatten (L.map (fun f -> f idx prf) (L.of_list rl)) 
      
  
end