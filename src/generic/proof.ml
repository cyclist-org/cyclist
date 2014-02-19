open Util

module Make(Node: Sigs.NODE) =
struct
  module Node = Node
  
  type t = (int * Node.t) Int.Map.t
  
  let get idx prf = Int.Map.find idx prf
  let find idx prf = snd (get idx prf)
  let fresh_idx prf = 1 + (fst (Int.Map.max_binding prf))
  let fresh_idxs xs prf = Blist.range (fresh_idx prf) xs 
  
  let size prf = Int.Map.cardinal prf
  let mem i p = Int.Map.mem i p

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
    Int.Map.for_all (fun _ (_,n) -> not (Node.is_open n)) prf

  let check p = 
    Soundcheck.check_proof 
      (Int.Map.map (fun (_,n) -> Node.to_abstract_node n) p)

  let no_of_backlinks p =
    size (Int.Map.filter (fun _ (_,n) -> Node.is_backlink n) p)

  let to_list m = Blist.map (fun (i,(_,n)) -> (i,n)) (Int.Map.bindings m)

  let ensure msg f = if (not f) then invalid_arg msg else () 
  
  let mk seq = Int.Map.add 0 (0,Node.mk_open seq) Int.Map.empty
  
  let replace idx n prf = Int.Map.add idx (fst (get idx prf),n) prf

  let ensure_add fn idx n prf =
    let n' = find idx prf in
    ensure fn (Node.is_open n'); 
    ensure fn (Node.Seq.equal (Node.get_seq n) (Node.get_seq n'))
    
  let add_axiom idx descr prf =
    let n = Node.mk_axiom (Node.get_seq (find idx prf)) descr in
    ensure_add "Proof.add_axiom" idx n prf;
    replace idx n prf    
      
  let add_backlink idx descr target vtts prf =
    let fn = "Proof.add_backlink" in
    let n = Node.mk_backlink (Node.get_seq (find idx prf)) descr target vtts in
    ensure_add fn idx n prf;
    ensure fn (mem target prf);
    replace idx n prf
  
  let add_abd idx descr prf =
    let cidx = fresh_idx prf in
    let seq = Node.get_seq (find idx prf) in
    let n = Node.mk_abd seq descr cidx in
    ensure_add "Proof.add_abd" idx n prf;
    (Int.Map.add cidx (idx, Node.mk_open seq) (replace idx n prf), cidx)
        
   let add_inf idx descr subgoals prf =
    let fn = "Proof.add_inf" in
    let subidxs = Blist.range (fresh_idx prf) subgoals in
    let subnodes = 
      Blist.map2 (fun i (seq,_,_) -> (i, Node.mk_open seq)) subidxs subgoals in
    let subidxs_plus_tags = 
      Blist.map2 (fun i (_,vtts,ptts) -> (i,vtts,ptts)) subidxs subgoals in
    let n = Node.mk_inf (Node.get_seq (find idx prf)) descr subidxs_plus_tags in
    ensure_add fn idx n prf;
    let prf' = 
      Blist.foldl 
        (fun prf' (ci,cn) -> Int.Map.add ci (idx,cn) prf') 
        (replace idx n prf) 
        subnodes in 
    (prf', subidxs)

  let get_ancestry idx prf =
    let rec aux acc idx (par_idx, n) =
      let parent = get par_idx prf in
      let acc = (par_idx, n)::acc in
      if par_idx=idx then acc else aux acc par_idx parent in
    aux [] idx (get idx prf)
    
        
end
