open Lib

module L = Blist

module Make(Seq: Sigs.SEQUENT)(Defs: Sigs.DEFS) =
  struct
    module Abdrule = Abdrule.Make(Seq)(Defs)
    module Proof = Proof.Make(Seq)
    module Node = Proofnode.Make(Seq)
    module Prover = Prover.Make(Seq)

    let print_proof_stats = Prover.print_proof_stats
    module Seq = Seq

    type seq_t = Seq.t
    type defs_t = Defs.t
    type abdrule_t = Abdrule.t
    type proof_t = Proof.t

    type app_state =
      {
        prf : Proof.t;
        depth : int;
        goals : (int * int) list;
        defs : Defs.t
      }
    let mk_app p d g defs = { prf=p; depth=d; goals=g; defs=defs }

    type abd_proof_state =
      {
        seq_no : int ;
        par : int ;
        idx : int;
        apps : app_state L.t
      }

    let state_seq_no = ref 0

    let mk_state par idx apps =
      {
        seq_no = (incr state_seq_no; !state_seq_no);
        par=par; idx=idx; apps=apps
      }

    let pop_parents sn stack =
      let rec loop aux s = function
        | [] -> Blist.rev aux
        | p::ps ->
            if p.seq_no = s then
              loop aux p.par ps
            else
              loop (p::aux) s ps in
      loop [] sn stack

    let expand_proof_state par_seq_no app rule =
      let () = assert (not (Proof.is_closed app.prf) && app.goals<>[]) in
      (* idx is the goal being closed and goal_depth is its depth *)
      let ((idx,goal_depth), goals) = Blist.decons app.goals in
      (* let () = assert (Node.is_open (Proof.find idx app.prf) && app.depth >= goal_depth) in *)
      let new_goal_depth = goal_depth+1 in
      let new_prf_depth = max app.depth new_goal_depth in
      let newapps =
        L.map
          begin fun ((g',p'),defs') ->
            mk_app
              p'
              new_prf_depth
              (Blist.rev_append (Blist.rev_map (fun j -> (j,new_goal_depth)) g') goals)
              defs'
          end
          (rule idx app.prf app.defs) in
      mk_state
        par_seq_no
        idx
        newapps


    let bfs maxbound rule seq initial_defs check =
      let rec aux bound frontier stack =
        if bound > maxbound || (stack = [] && frontier = []) then None else
        if stack=[] then
          (* finished current depth, increase and repeat *)
          aux (bound + 1) [] (Blist.rev frontier)
        else
        let (proof_state, stack) = Blist.decons stack in
        (* if no applications left, go to next set of applications *)
        if L.is_empty proof_state.apps then aux bound frontier stack else
        (* next rule application *)
        let (app, apps) = Blist.decons proof_state.apps in
        let () = assert (app.depth <= bound) in
        let () = assert (Blist.for_all (fun (_,gd) -> gd <= bound) app.goals) in
        (* push remaining applications *)
        let stack = {proof_state with apps=apps} :: stack in
        if app.goals=[] then
          begin
            (* no subgoals left, so it must be a closed proof *)
            assert (Proof.is_closed app.prf) ;
            if check app.defs then Some (app.prf,app.depth,app.defs) else
            (* NOTE: in case not acceptable we do not pop parents as we may need to backtrack *)
            aux bound frontier stack
          end else
        let () = assert (not (Proof.is_closed app.prf)) in
        let () = if !do_debug then
          begin
            print_endline ("Expanding node: " ^ (string_of_int (fst (Blist.hd app.goals)))) ;
            print_endline (Proof.to_string app.prf)
          end in
        if Blist.exists (fun (_,gd) -> gd = bound) app.goals then
          begin
            (* if any of the open goals is at the current depth *)
            (* then keep for later *)
            let frontier = (expand_proof_state proof_state.seq_no app rule) :: frontier in
            aux bound frontier stack
          end else
        let stack = if Proof.is_closed_at app.prf proof_state.idx  then
          (* last application resulted in no new open subgoals *)
          (* thus we will pop all generators of applications *)
          (* that are parents of the current one *)
          (* this is equivalent to a prolog cut over the other possible *)
          (* closed proofs of these goals *)
          pop_parents proof_state.seq_no stack
        else
          stack in
        let stack = (expand_proof_state proof_state.seq_no app rule) :: stack in
        aux bound frontier stack in
      let start = Proof.mk seq in
      let stack =
        [expand_proof_state 0 (mk_app start 0 [(0,0)] initial_defs) rule] in
      Option.map
        (fun (p, d, defs) -> Prover.last_search_depth := d ; (p, defs))
        (aux 1 [] stack)
  end
