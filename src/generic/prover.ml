open Lib
open Symbols

(* module L = Zlist *)
module L = Blist

module type S = sig
  type rule_t

  module Seq : Sequent.S

  module Proof : Proof.S

  val last_search_depth : int ref

  val idfs : int -> int -> rule_t -> rule_t -> Seq.t -> Proof.t option

  (* val bfs : int -> rule_t -> rule_t -> Seq.t -> Proof.t option   *)
  val print_proof_stats : Proof.t -> unit
end

module Make (Seq : Sequent.S) = struct
  module Proof = Proof.Make (Seq)
  module Node = Proofnode.Make (Seq)
  module Rule = Proofrule.Make (Seq)
  module Seqtactics = Seqtactics.Make (Seq)

  type proof_t = Proof.t

  type rule_t = Rule.t

  module Seq = Seq

  (* due to divergence between tree depth and search depth *)
  (* remember last successful search depth *)
  let last_search_depth = ref 0

  let rec idfs bound maxbound ax r seq =
    if Int.( > ) bound maxbound then None
    else
      let rec dfs bound idx prf =
        if Int.( < ) bound 0 then None
        else
          let () =
            debug (fun () ->
                "Trying to close node: " ^ string_of_int idx ^ "\n"
                ^ Proof.to_string prf ^ "\n" )
          in
          let res =
            Option.map snd
              (L.find_opt (fun (ss', _) -> Blist.is_empty ss') (ax idx prf))
          in
          if Option.is_some res then res
          else
            L.find_map
              (fun (subgoals', prf') ->
                Blist.fold_left
                  (fun optprf idx' -> Option.bind (dfs (bound - 1) idx') optprf)
                  (Some prf') subgoals' )
              (r idx prf)
      in
      match dfs bound 0 (Proof.mk seq) with
      | None -> idfs (bound + 1) maxbound ax r seq
      | res ->
          last_search_depth := bound ;
          res

  let print_proof_stats proof =
    let size = Proof.size proof in
    let links = Proof.num_backlinks proof in
    print_endline
      ( "Proof has " ^ string_of_int size ^ " nodes" ^ " and "
      ^ string_of_int links ^ " back-links." ) ;
    print_endline
      ("Required search depth was " ^ string_of_int !last_search_depth)

  (* type app_state =                                                                              *)
  (*   {                                                                                           *)
  (*     prf : Proof.t;                                                                            *)
  (*     depth : int;                                                                              *)
  (*     goals : (int * int) list;                                                                 *)
  (*   }                                                                                           *)
  (* let mk_app p d g = { prf=p; depth=d; goals=g }                                                *)
  
  (* type proof_state =                                                                            *)
  (*   {                                                                                           *)
  (*     seq_no : int ;                                                                            *)
  (*     par : int ;                                                                               *)
  (*     idx : int;                                                                                *)
  (*     apps : app_state L.t                                                                      *)
  (*   }                                                                                           *)
  
  (* let state_seq_no = ref 0                                                                      *)
  
  (* let mk_state par idx apps =                                                                   *)
  (*   {                                                                                           *)
  (*     seq_no = (incr state_seq_no; !state_seq_no);                                              *)
  (*     par=par; idx=idx; apps=apps                                                               *)
  (*   }                                                                                           *)
  
  (* let pop_parents sn stack =                                                                    *)
  (*   let rec loop aux s = function                                                               *)
  (*     | [] -> Blist.rev aux                                                                     *)
  (*     | p::ps ->                                                                                *)
  (*         if p.seq_no = s then                                                                  *)
  (*           loop aux p.par ps                                                                   *)
  (*         else                                                                                  *)
  (*           loop (p::aux) s ps in                                                               *)
  (*   loop [] sn stack                                                                            *)
  
  (* let expand_proof_state par_seq_no app rule =                                                  *)
  (*   let () = assert (not (Proof.is_closed app.prf) && app.goals<>[]) in                         *)
  (*   (* idx is the goal being closed and goal_depth is its depth *)                              *)
  (*   let ((idx,goal_depth), goals) = Blist.decons app.goals in                                   *)
  (*   (* let () = assert (Node.is_open (Proof.find idx app.prf) && app.depth >= goal_depth) in *) *)
  (*   let new_goal_depth = goal_depth+1 in                                                        *)
  (*   let new_prf_depth = max app.depth new_goal_depth in                                         *)
  (*   let newapps =                                                                               *)
  (*     L.map                                                                                     *)
  (*       begin fun (g',p') ->                                                                    *)
  (*         mk_app                                                                                *)
  (*           p'                                                                                  *)
  (*           new_prf_depth                                                                       *)
  (*           (Blist.rev_append (Blist.rev_map (fun j -> (j,new_goal_depth)) g') goals)           *)
  (*       end                                                                                     *)
  (*       (rule idx app.prf) in                                                                   *)
  (*   mk_state                                                                                    *)
  (*     par_seq_no                                                                                *)
  (*     idx                                                                                       *)
  (*     newapps                                                                                   *)
  
  (* let bfs maxbound ax rl seq =                                                                  *)
  (*   let rule =  Rule.first [ ax ; Rule.compose rl (Rule.attempt ax) ] in                        *)
  (*   let rec aux bound frontier stack =                                                          *)
  (*     if bound > maxbound || (stack = [] && frontier = []) then None else                       *)
  (*     if stack=[] then                                                                          *)
  (*       (* finished current depth, increase and repeat *)                                       *)
  (*       aux (bound + 1) [] (Blist.rev frontier)                                                 *)
  (*     else                                                                                      *)
  (*     let (proof_state, stack) = Blist.decons stack in                                          *)
  (*     (* if no applications left, go to next set of applications *)                             *)
  (*     if L.is_empty proof_state.apps then aux bound frontier stack else                         *)
  (*     (* next rule application *)                                                               *)
  (*     let (app, apps) = Blist.decons proof_state.apps in                                        *)
  (*     let () = assert (app.depth <= bound) in                                                   *)
  (*     let () = assert (Blist.for_all (fun (_,gd) -> gd <= bound) app.goals) in                  *)
  (*     (* push remaining applications *)                                                         *)
  (*     let stack = {proof_state with apps=apps} :: stack in                                      *)
  (*     if app.goals=[] then                                                                      *)
  (*       begin                                                                                   *)
  (*         (* no subgoals left, so it must be a closed proof *)                                  *)
  (*         assert (Proof.is_closed app.prf) ;                                                    *)
  (*         Some (app.prf,app.depth)                                                              *)
  (*       end else                                                                                *)
  (*     let () = assert (not (Proof.is_closed app.prf)) in                                        *)
  (*     let () = debug (fun () ->                                                                 *)
  (*       "Trying to close node: " ^ (string_of_int (fst (Blist.hd app.goals))) ^ "\n" ^          *)
  (*       (Proof.to_string app.prf) ^ "\n"                                                        *)
  (*       ) in                                                                                    *)
  (*     if Blist.exists (fun (_,gd) -> gd = bound) app.goals then                                 *)
  (*       begin                                                                                   *)
  (*         (* if any of the open goals is at the current depth *)                                *)
  (*         (* then keep for later *)                                                             *)
  (*         let frontier = (expand_proof_state proof_state.seq_no app rule) :: frontier in        *)
  (*         aux bound frontier stack                                                              *)
  (*       end else                                                                                *)
  (*     let stack = if Proof.is_closed_at app.prf proof_state.idx  then                           *)
  (*       (* last application resulted in no new open subgoals *)                                 *)
  (*       (* thus we will pop all generators of applications *)                                   *)
  (*       (* that are parents of the current one *)                                               *)
  (*       (* this is equivalent to a prolog cut over the other possible *)                        *)
  (*       (* closed proofs of these goals *)                                                      *)
  (*       pop_parents proof_state.seq_no stack                                                    *)
  (*     else                                                                                      *)
  (*       stack in                                                                                *)
  (*     let stack = (expand_proof_state proof_state.seq_no app rule) :: stack in                  *)
  (*     aux bound frontier stack in                                                               *)
  (*   let start = Proof.mk seq in                                                                 *)
  (*   let stack =                                                                                 *)
  (*     [expand_proof_state 0 (mk_app start 0 [(0,0)]) rule] in                                   *)
  (*   Option.map                                                                                  *)
  (*     (fun (p, d) -> last_search_depth := d ; p)                                                *)
  (*     (aux 1 [] stack)                                                                          *)
end
