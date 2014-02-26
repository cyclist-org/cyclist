open Util

module Make(Proof: Sigs.PROOF) =
struct
  module Proof = Proof
  module Node = Proof.Node
  module Seq = Node.Seq

  type axiom_f = Seq.t -> string option
  
  let mk_axiom ax_f idx prf =
    match ax_f (Proof.get_seq idx prf) with
    | None -> []
    | Some descr -> [(Proof.add_axiom idx descr prf, [])]  

  type infrule_app = (Seq.t * Util.TagPairs.t * Util.TagPairs.t) list * string
  type infrule_f = Seq.t -> infrule_app list
  type infrule = int -> Proof.t -> (Proof.t * int list) list
  
  let mk_infrule r_f idx prf =
    let seq = Proof.get_seq idx prf in
    let mk (l,d) = Proof.add_inf idx d l prf in
    Blist.map mk (r_f seq)

  type backrule_f = Seq.t -> Seq.t -> (Util.TagPairs.t * string) list
  type select_f = int -> Proof.t -> int list
        
  let mk_backrule sel_f br_f srcidx prf =
    let srcseq = Proof.get_seq srcidx prf in
    let trgidxs = sel_f srcidx prf in
    let mk trgidx (vtts,d) = 
      (Proof.add_backlink srcidx d trgidx vtts prf,[]) in
    let check (p,_) = Proof.check p in
    let apply trgidx = 
      let trgseq = Proof.get_seq trgidx prf in
      Blist.map (mk trgidx) (br_f srcseq trgseq) in
    Blist.filter check (Blist.flatten (Blist.map apply trgidxs))
    
  let all_nodes srcidx prf = 
    Blist.filter (fun idx -> idx<>srcidx) (Blist.map fst (Proof.to_list prf))
  let ancestor_nodes srcidx prf = Blist.map fst (Proof.get_ancestry srcidx prf)

end