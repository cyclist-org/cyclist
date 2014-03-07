open Lib
open Util
open Symbols

module L = Zlist

module Make(Seq: Sigs.SEQUENT) =
  struct
    module Proof = Proof.Make(Seq)
    (* module Rules = Proofrules.Make(Seq) *)
    module Seqtactics = Seqtactics.Make(Seq)
    
    type proof_t = Proof.t
    type rule_t = Proofrule.Make(Seq).t

    module Seq = Seq    
            
    (* due to divergence between tree depth and search depth *)
    (* remember last successful search depth *)
    let last_search_depth = ref 0

    let rec dfs bound r idx prf =
      if bound<0 then None else
      let apps = r idx prf in
      let res = Option.map snd (L.find_first (fun (ss', _) -> ss'=[]) apps) in
      if Option.is_some res then res else
      L.find_some
        (fun (subgoals', prf') -> 
          Blist.fold_left
            (fun optprf idx' -> Option.bind (dfs (bound-1) r idx') optprf)
            (Some prf') 
            subgoals')
        apps
    
    let rec idfs bound maxbound r seq =
      if bound>maxbound then None else
      match dfs bound r 0 (Proof.mk seq) with
      | None -> idfs (bound+1) maxbound r seq
      | res -> last_search_depth := bound ; res
          

    let melt_proof ch p =
      ignore (Latex.to_channel ~mode:Latex.M ch (Proof.to_melt p))
    (* print stats on stdout *)
    let print_proof_stats proof =
      let size = Proof.size proof in
      (* let depth = depth_of_proof Proof.t in *)
      let links = Proof.no_of_backlinks proof in
      print_endline
        ("Proof has " ^ (string_of_int size) ^
         " nodes and a depth of " ^ (string_of_int !last_search_depth) ^
         " and " ^ (string_of_int links) ^ " back-links.")

  end
