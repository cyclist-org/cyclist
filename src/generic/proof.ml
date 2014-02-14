open Util

module Make(Node: Sigs.NODE) =
struct
  module Node = Node
  type seq_t = Node.seq_t
  type node_t = Node.t
  type t = node_t Int.Map.t
  
  let find idx (prf:t) = Int.Map.find idx prf
  let add pidx (nd:Node.t) (prf:t) = Int.Map.add pidx nd prf
  let fresh_idx (prf:t) = 1 + (fst (Int.Map.max_binding prf))
  let empty:t = Int.Map.empty
  let map f (prf:t) = Int.Map.map f prf
  let filter f (prf:t) = Int.Map.filter f prf
  let for_all f (prf:t) = Int.Map.for_all f prf
  let size = Int.Map.cardinal
  let mem i (p:t) = Int.Map.mem i p
  let is_empty (p:t) = Int.Map.is_empty p

  let get_ancestry idx prf =
    let rec aux acc idx n =
      let par_idx = Node.get_par n in
      let parent = find par_idx prf in
      let acc = add par_idx parent acc in
      if par_idx=idx then acc else aux acc par_idx parent in
    aux empty idx (find idx prf)

  let abstract prf = map Node.to_abstract_node prf

  let check p = Soundcheck.check_proof (abstract p)

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
 
  (* FIXME lazy soundness checking de-implemented *) 
  let is_closed prf =
    Int.Map.for_all (fun _ n -> not (Node.is_open n)) prf

          
  let no_of_backlinks p =
    size (Int.Map.filter (fun _ n -> Node.is_backlink n) p)

  let to_list m = Int.Map.bindings m
  
  let singleton idx n = Int.Map.add idx n empty
end
