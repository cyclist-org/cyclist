open Util

module Make(Node: Sigs.NODE) =
struct
  module Node = Node
  
  type t = Node.t Int.Map.t
  
  let find idx prf = Int.Map.find idx prf
  let fresh_idx prf = 1 + (fst (Int.Map.max_binding prf))
  let fresh_idxs xs prf = Blist.range (fresh_idx prf) xs 
  
  let empty = Int.Map.empty
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
    Int.Map.for_all (fun _ n -> not (Node.is_open n)) prf

  let check p = 
    Soundcheck.check_proof (Int.Map.map Node.to_abstract_node p)

  let no_of_backlinks p =
    size (Int.Map.filter (fun _ n -> Node.is_backlink n) p)

  let to_list m = Int.Map.bindings m

  let ensure msg f = if (not f) then invalid_arg msg else () 
  
  let mk n =
    let ensure = ensure "Proof.mk" in
    ensure (Node.is_open n || Node.is_axiom n);
    ensure (Node.get_par n == 0);
    Int.Map.add 0 n empty

  let ensure_add fn idx n prf =
    let n' = find idx prf in
    let ensure = ensure fn in
    ensure (Node.is_open n'); 
    ensure (Node.get_par n == Node.get_par n');
    ensure (Node.Seq.equal (Node.get_seq n) (Node.get_seq n'))
    
  let add_backlink idx n prf =
    let fn = "Proof.add_backlink" in
    let ensure = ensure fn in
    ensure_add fn idx n prf;
    ensure (Node.is_backlink n);
    ensure (mem (Blist.hd (Node.get_succs n)) prf);
    Int.Map.add idx n prf

  let add_abd idx n (child_idx, child_n) prf =
    let fn = "Proof.add_backlink" in
    let ensure = ensure fn in
    ensure_add fn idx n prf;
    ensure (Node.is_abd n);
    ensure ((Blist.hd (Node.get_succs n)) == child_idx);
    ensure (not (mem child_idx prf));
    ensure (Node.is_open child_n || Node.is_axiom child_n);    
    Int.Map.add child_idx child_n (Int.Map.add idx n prf)
    
  let add_inf idx n subgoals prf = 
    let fn = "Proof.add_inf" in
    let ensure = ensure fn in
    ensure_add fn idx n prf;
    ensure (Node.is_inf n);
    ensure (Int.FList.equal (Node.get_succs n) (fst (Blist.split subgoals)));
    ensure (Blist.for_all (fun i' -> not (mem i' prf)) (Node.get_succs n));
    Blist.iter 
      (fun (ci,cn) -> 
        ensure (Node.is_open cn || Node.is_axiom cn);
        ensure (not (mem ci prf));
        ensure (Node.get_par cn == idx);
      )
      subgoals;
    Blist.foldl 
      (fun p' (i',n') -> Int.Map.add i' n' p') 
      prf 
      ((idx,n)::subgoals) 
    
end
