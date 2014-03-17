open Util

module Make(Seq : Sigs.SEQUENT) =
struct
  module Node = Proofnode.Make(Seq)
  
  module P =
    struct 
      type 'a t = 'a Int.Map.t
      let find = Int.Map.find
      let max_binding = Int.Map.max_binding
      let cardinal = Int.Map.cardinal
      let mem = Int.Map.mem
      let for_all = Int.Map.for_all
      let map = Int.Map.map
      let bindings = Int.Map.bindings
      let add = Int.Map.add
      let empty = Int.Map.empty
    end
  
  type t = (int * Node.t) P.t
  type seq_t = Seq.t
  type node_t = Node.t
  
  let get idx prf = P.find idx prf
  let find idx prf = snd (get idx prf)
  let get_seq idx prf = Node.get_seq (find idx prf)
  let fresh_idx prf = 1 + (fst (P.max_binding prf))
  let fresh_idxs xs prf = Blist.range (fresh_idx prf) xs 
  
  let size prf = P.cardinal prf
  
  (* not exported *)
  let mem i p = P.mem i p

  let pp fmt prf =
    let rec pp_proof_node fmt id =
      Node.pp fmt id (find id prf) pp_proof_node in
    Format.fprintf fmt "@[%a@]@\n" pp_proof_node 0

  let to_string prf =
    ignore (Format.flush_str_formatter ());
    Format.pp_set_margin Format.str_formatter 300;
    Format.fprintf Format.str_formatter "@[%a@]" pp prf ;
    Format.flush_str_formatter ()

  let to_melt proof =
    let rec melt_proof_node first id =
      Node.to_melt first id (find id proof) melt_proof_node in
    melt_proof_node true 0
 
  let is_closed prf =
    P.for_all (fun _ (_,n) -> not (Node.is_open n)) prf

  let check p = 
    Soundcheck.check_proof 
      (P.map (fun (_,n) -> Node.to_abstract_node n) p)

  let to_list m = Blist.map (fun (i,(_,n)) -> (i,n)) (P.bindings m)

  let ensure msg f = if (not f) then invalid_arg msg else () 
  
  let mk seq = P.add 0 (0,Node.mk_open seq) P.empty
  
  let replace idx n prf = P.add idx (fst (get idx prf),n) prf

  let ensure_add fn idx n prf =
    let n' = find idx prf in
    ensure fn (Node.is_open n'); 
    ensure fn (Seq.equal (Node.get_seq n) (Node.get_seq n'))
    
  let add_axiom idx descr prf =
    let n = Node.mk_axiom (get_seq idx prf) descr in
    ensure_add "Proof.add_axiom" idx n prf;
    replace idx n prf    
      
  let add_backlink idx descr target vtts prf =
    let fn = "Proof.add_backlink" in
    let n = Node.mk_backlink (get_seq idx prf) descr target vtts in
    ensure_add fn idx n prf;
    ensure fn (mem target prf);
    replace idx n prf
  
   let add_inf idx descr subgoals prf =
    let fn = "Proof.add_inf" in
    let subidxs = Blist.range (fresh_idx prf) subgoals in
    let subnodes = 
      Blist.map2 (fun i (seq,_,_) -> (i, Node.mk_open seq)) subidxs subgoals in
    let subidxs_plus_tags = 
      Blist.map2 (fun i (_,vtts,ptts) -> (i,vtts,ptts)) subidxs subgoals in
    let n = Node.mk_inf (get_seq idx prf) descr subidxs_plus_tags in
    ensure_add fn idx n prf;
    let prf' = 
      Blist.foldl 
        (fun prf' (ci,cn) -> P.add ci (idx,cn) prf') 
        (replace idx n prf) 
        subnodes in 
    (subidxs, prf')

  let get_ancestry idx prf =
    let rec aux acc idx (par_idx, n) =
      let parent = get par_idx prf in
      let acc = (par_idx, n)::acc in
      if par_idx=idx then acc else aux acc par_idx parent in
    aux [] idx (get idx prf)
    
        
end
