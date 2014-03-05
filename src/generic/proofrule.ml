open Util

module Make(Seq : Sigs.SEQUENT) =
struct
  type seq_t = Seq.t
  module Proof = Proof.Make(Seq)
  type proof_t = Proof.t

  type axiom_f = seq_t -> string option
  
  let mk_axiom ax_f idx prf =
    match ax_f (Proof.get_seq idx prf) with
    | None -> []
    | Some descr -> [([], Proof.add_axiom idx descr prf)]  

  type infrule_app = (seq_t * Util.TagPairs.t * Util.TagPairs.t) list * string
  type infrule_f = seq_t -> infrule_app list
  type t = int -> Proof.t -> (int list * Proof.t) list
  
  let mk_infrule r_f idx prf =
    let seq = Proof.get_seq idx prf in
    let mk (l,d) = Proof.add_inf idx d l prf in
    Blist.map mk (r_f seq)

  type backrule_f = seq_t -> seq_t -> (Util.TagPairs.t * string) list
  type select_f = int -> Proof.t -> int list
        
  let mk_backrule greedy sel_f br_f srcidx prf =
    let srcseq = Proof.get_seq srcidx prf in
    let trgidxs = sel_f srcidx prf in
    let mk trgidx (vtts,d) = 
      ([], Proof.add_backlink srcidx d trgidx vtts prf) in
    let check (_,p) = Proof.check p in
    let apply trgidx = 
      let trgseq = Proof.get_seq trgidx prf in
      Blist.map (mk trgidx) (br_f srcseq trgseq) in
    if greedy then
      begin
        let rsl = 
          Blist.find_first 
            (Option.pred check)  
            (Blist.flatten (Blist.map apply trgidxs)) in
        match rsl with
        | None -> []
        | Some rsl' -> [rsl']
      end
    else  
      Blist.filter check (Blist.flatten (Blist.map apply trgidxs))
    
  
  let all_nodes srcidx prf = 
    Blist.filter (fun idx -> idx<>srcidx) (Blist.map fst (Proof.to_list prf))
  let ancestor_nodes srcidx prf = Blist.map fst (Proof.get_ancestry srcidx prf)

  let fail _ _ = []

    let apply_to_subgoals r (subgoals, prf) =
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
                (r idx oldprf))
          apps))
      [ ([], prf) ]
      subgoals

  let compose r r' idx prf =
    Blist.flatten (Blist.map (apply_to_subgoals r') (r idx prf))

  let choice rl idx prf =
    Blist.flatten (Blist.map (fun f -> f idx prf) rl) 
      
  
end