open Lib

module L = Blist

module type S = sig

  type seq_t
  type rule_t
  type proof_t

  val last_search_depth : int ref

  val idfs : int -> int -> rule_t -> rule_t -> seq_t -> proof_t option

  (* val bfs : int -> rule_t -> rule_t -> Seq.t -> Proof.t option   *)
  val print_proof_stats : proof_t -> unit
  val pp_proof_stats : Format.formatter -> proof_t -> unit
end

module Make (Seq : Sequent.S) (Infrule : Infrule.S) = struct

  module Proof = Proof.Make(Seq)(Infrule)
  module Rule = Proofrule.Make(Seq)(Infrule)

  (* due to divergence between tree depth and search depth *)
  (* remember last successful search depth *)
  let last_search_depth = ref 0

  let rec idfs bound maxbound ax r seq =
    (* DFS returns one of three options:
         - None: Didn't find a proof yet (i.e. some open subgoals remain);
         - Some None: Didn't find a proof, and exhausted the search space;
         - Some (Some prf): We found a proof [prf]. *)
    let rec dfs bound idx prf =
      if Int.( <= ) bound 0 then
        None
      else
        let () =
          debug (fun () ->
            Format.asprintf "Trying to close node: %i@.%a@."
              idx
              Proof.pp prf) in
        let res =
          Option.map snd
            (L.find_opt (fun (ss', _) -> Blist.is_empty ss') (ax idx prf))
        in
        if Option.is_some res then
          Some res
        else
          match (r idx prf) with
          | [] ->
            Some None
          | apps ->
            let res =
              L.find_map_or
                (fun (subgoals', prf') ->
                  Blist.fold_left
                    (fun prf idx' ->
                      match prf with
                      | None ->
                        (* The search to close some earlier node hit the max
                           search depth without exhausting the search space,
                           so we need to continue trying to close the next
                           open node in case this reveals a dead-end in the
                           search space. *)
                        dfs (bound - 1) idx' prf'
                      | Some None ->
                        (* The search to close some earlier node exhausted the
                           search space, so we simply propagate this result. *)
                        prf
                      | Some (Some prf) ->
                        (* We have found subproofs for all previous nodes in
                           this proof, so continue with the next open node. *)
                        dfs (bound - 1) idx' prf)
                    (Some (Some prf'))
                    (subgoals'))
                (function
                  | Some (Some prf) when Proof.is_closed_at prf idx -> true
                  | _ -> false)
                (apps) in
            match res with
            | Left prf ->
              (* In this case, we are guaranteed by the definition of
                 find_map_or that prf is of the form (Some (Some prf')) with
                 prf' closed at idx. *)
              prf
            | Right attempts ->
              (* Otherwise we need to check whether proof search for each of the
                 different rule applications exhausted the search space. *)
              if
                List.for_all
                  (function | Some None -> true | _ -> false)
                  (attempts)
              (* In which case, we have exhausted the search space overall. *)
              then Some None
              (* Otherwise, there was some branch of the search space that can
                 still to be explored further. *)
              else None in
    if Int.( > ) bound maxbound then
      None
    else
      let () =
        debug (fun _ ->
          Format.asprintf "Beginning proof search up to depth %i" bound) in
      let () = last_search_depth := bound in
      match dfs bound 0 (Proof.mk seq) with
      | None ->
        idfs (bound + 1) maxbound ax r seq
      | Some res ->
        res

  let pp_proof_stats fmt prf =
    let prf_size = Proof.size prf in
    Format.fprintf fmt "Proof has %i node%s and %i back-links."
      (prf_size)
      (if Int.(prf_size > 1) then "s" else "")
      (Proof.num_backlinks prf) ;
    Format.pp_print_newline fmt ()

  let print_proof_stats prf =
    pp_proof_stats Format.std_formatter prf

  (* type app_state =                                                         *)
  (*   {                                                                      *)
  (*     prf : Proof.t;                                                       *)
  (*     depth : int;                                                         *)
  (*     goals : (int * int) list;                                            *)
  (*   }                                                                      *)
  (* let mk_app p d g = { prf=p; depth=d; goals=g }                           *)

  (* type proof_state =                                                       *)
  (*   {                                                                      *)
  (*     seq_no : int ;                                                       *)
  (*     par : int ;                                                          *)
  (*     idx : int;                                                           *)
  (*     apps : app_state L.t                                                 *)
  (*   }                                                                      *)

  (* let state_seq_no = ref 0                                                 *)

  (* let mk_state par idx apps =                                              *)
  (*   {                                                                      *)
  (*     seq_no = (incr state_seq_no; !state_seq_no);                         *)
  (*     par=par; idx=idx; apps=apps                                          *)
  (*   }                                                                      *)

  (* let pop_parents sn stack =                                               *)
  (*   let rec loop aux s = function                                          *)
  (*     | [] -> Blist.rev aux                                                *)
  (*     | p::ps ->                                                           *)
  (*         if p.seq_no = s then                                             *)
  (*           loop aux p.par ps                                              *)
  (*         else                                                             *)
  (*           loop (p::aux) s ps in                                          *)
  (*   loop [] sn stack                                                       *)

  (* module Node = Proofnode.Make (Seq) *)

  (* let expand_proof_state par_seq_no app rule =                             *)
  (*   let () = assert (not (Proof.is_closed app.prf) && app.goals<>[]) in    *)
  (*   (* idx is the goal being closed and goal_depth is its depth *)         *)
  (*   let ((idx,goal_depth), goals) = Blist.decons app.goals in              *)
  (*   (* let () =                                                            *)
  (*        assert (Node.is_open (Proof.find idx app.prf)) in                 *)
  (*      let () = assert (app.depth >= goal_depth) in *)                     *)
  (*   let new_goal_depth = goal_depth+1 in                                   *)
  (*   let new_prf_depth = max app.depth new_goal_depth in                    *)
  (*   let newapps =                                                          *)
  (*     L.map                                                                *)
  (*       begin fun (g',p') ->                                               *)
  (*         mk_app                                                           *)
  (*           p'                                                             *)
  (*           new_prf_depth                                                  *)
  (*           (Blist.rev_append                                              *)
  (*             (Blist.rev_map (fun j -> (j,new_goal_depth)) g') goals)      *)
  (*       end                                                                *)
  (*       (rule idx app.prf) in                                              *)
  (*   mk_state                                                               *)
  (*     par_seq_no                                                           *)
  (*     idx                                                                  *)
  (*     newapps                                                              *)

  (* let bfs maxbound ax rl seq =                                             *)
  (*   let rule =  Rule.first [ ax ; Rule.compose rl (Rule.attempt ax) ] in   *)
  (*   let rec aux bound frontier stack =                                     *)
  (*     if bound > maxbound || (stack = [] && frontier = []) then None else  *)
  (*     if stack=[] then                                                     *)
  (*       (* finished current depth, increase and repeat *)                  *)
  (*       aux (bound + 1) [] (Blist.rev frontier)                            *)
  (*     else                                                                 *)
  (*     let (proof_state, stack) = Blist.decons stack in                     *)
  (*     (* if no applications left, go to next set of applications *)        *)
  (*     if L.is_empty proof_state.apps then aux bound frontier stack else    *)
  (*     (* next rule application *)                                          *)
  (*     let (app, apps) = Blist.decons proof_state.apps in                   *)
  (*     let () = assert (app.depth <= bound) in                              *)
  (*     let () =                                                             *)
  (*       assert (Blist.for_all (fun (_,gd) -> gd <= bound) app.goals) in    *)
  (*     (* push remaining applications *)                                    *)
  (*     let stack = {proof_state with apps=apps} :: stack in                 *)
  (*     if app.goals=[] then                                                 *)
  (*       begin                                                              *)
  (*         (* no subgoals left, so it must be a closed proof *)             *)
  (*         assert (Proof.is_closed app.prf) ;                               *)
  (*         Some (app.prf,app.depth)                                         *)
  (*       end else                                                           *)
  (*     let () = assert (not (Proof.is_closed app.prf)) in                   *)
  (*     let () = debug (fun () ->                                            *)
  (*       "Trying to close node: " ^                                         *)
  (*       (string_of_int (fst (Blist.hd app.goals))) ^ "\n" ^                *)
  (*       (Proof.to_string app.prf) ^ "\n"                                   *)
  (*       ) in                                                               *)
  (*     if Blist.exists (fun (_,gd) -> gd = bound) app.goals then            *)
  (*       begin                                                              *)
  (*         (* if any of the open goals is at the current depth *)           *)
  (*         (* then keep for later *)                                        *)
  (*         let frontier =                                                   *)
  (*           expand_proof_state proof_state.seq_no app rule) :: frontier in *)
  (*         aux bound frontier stack                                         *)
  (*       end else                                                           *)
  (*     let stack = if Proof.is_closed_at app.prf proof_state.idx  then      *)
  (*       (* last application resulted in no new open subgoals *)            *)
  (*       (* thus we will pop all generators of applications *)              *)
  (*       (* that are parents of the current one *)                          *)
  (*       (* this is equivalent to a prolog cut over the other possible *)   *)
  (*       (* closed proofs of these goals *)                                 *)
  (*       pop_parents proof_state.seq_no stack                               *)
  (*     else                                                                 *)
  (*       stack in                                                           *)
  (*     let stack =                                                          *)
  (*       (expand_proof_state proof_state.seq_no app rule) :: stack in       *)
  (*     aux bound frontier stack in                                          *)
  (*   let start = Proof.mk seq in                                            *)
  (*   let stack =                                                            *)
  (*     [expand_proof_state 0 (mk_app start 0 [(0,0)]) rule] in              *)
  (*   Option.map                                                             *)
  (*     (fun (p, d) -> last_search_depth := d ; p)                           *)
  (*     (aux 1 [] stack)                                                     *)
end
