open Lib
open Util
open Symbols

(* module L = Zlist *)
module L = Blist

module Make(Seq: Sigs.SEQUENT) =
  struct
    module Proof = Proof.Make(Seq)
    module Node = Proofnode.Make(Seq)
    module Rule = Proofrule.Make(Seq)
    module Seqtactics = Seqtactics.Make(Seq)
    
    type proof_t = Proof.t
    type rule_t = Rule.t

    module Seq = Seq    
            
    (* due to divergence between tree depth and search depth *)
    (* remember last successful search depth *)
    let last_search_depth = ref 0
    
    let rec idfs bound maxbound ax r seq =
      if bound>maxbound then None else
      let rec dfs bound idx prf =
        if bound<0 then None else
        let () = debug (fun () ->
          "Trying to close node: " ^ (string_of_int idx) ^ "\n" ^
          (Proof.to_string prf) ^ "\n"
          ) in
        let res = 
          Option.map snd (L.find_first (fun (ss', _) -> ss'=[]) (ax idx prf)) in
        if Option.is_some res then res else
        L.find_some
          (fun (subgoals', prf') -> 
            Blist.fold_left
              (fun optprf idx' -> Option.bind (dfs (bound-1) idx') optprf)
              (Some prf') 
              subgoals')
          (r idx prf) in
      match dfs bound 0 (Proof.mk seq) with
      | None -> idfs (bound+1) maxbound ax r seq
      | res -> last_search_depth := bound ; res
          
          
    let melt_proof ch p =
      ignore (Latex.to_channel ~mode:Latex.M ch (Proof.to_melt p))
    let print_proof_stats proof =
      let size = Proof.size proof in
      let links = 
        Blist.length (
          Blist.filter (fun (_,n) -> Node.is_backlink n) (Proof.to_list proof)) in
      print_endline
        ("Proof has " ^ (string_of_int size) ^
         " nodes and a depth of " ^ (string_of_int !last_search_depth) ^
         " and " ^ (string_of_int links) ^ " back-links.")

  end
