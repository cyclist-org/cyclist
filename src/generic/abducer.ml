open Lib

module Make(Seq: Sigs.SEQUENT)(Defs: Sigs.DEFS) =
  struct
    module Abdrule = Abdrule.Make(Seq)(Defs)
    module Proof = Proof.Make(Seq)
    module Node = Proofnode.Make(Seq)
    module Prover = Prover2.Make(Seq)
    
    let melt_proof = Prover.melt_proof
    let print_proof_stats = Prover.print_proof_stats
    module Seq = Seq
    
    type seq_t = Seq.t
    type defs_t = Defs.t
    type abdrule_t = Abdrule.t
    type proof_t = Proof.t

    let last_search_depth = ref 0

    let is_closed_at idx prf =
      let rec aux idx' =
        let n = Proof.find idx' prf in
        if Node.is_axiom n then true else
        if Node.is_open n then false else
        if Node.is_backlink n then true else
        Blist.for_all aux (Node.get_succs n) in 
      aux idx

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
            apps : app_state Zlist.t
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
        Zlist.map
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

    exception Continue
    
    let bfs minbound maxbound rule seq initial_defs check =
      let bound = ref minbound in
      let start = Proof.mk seq in
      let stack = ref [expand_proof_state 0 (mk_app start 0 [(0,0)] initial_defs) rule] in
      let found = ref None in
      let frontier = ref [] in
      while !bound <= maxbound && Option.is_none !found &&
        (!stack <> [] || !frontier <> []) do
        try
          if !stack=[] then
            begin
              (* finished current depth, increase and Blist.repeat *)
              incr bound;
              stack := Blist.rev !frontier;
              frontier := [];
              raise Continue
            end ;
          (* idx points to node being closed *)
          (* let ((_, idx) as par, next) = Blist.hd !stack in *)
          let proof_state = Blist.hd !stack in
          let () = stack := Blist.tl !stack in
          (* if no applications left, go to next set of applications *)
          if Zlist.is_empty proof_state.apps then raise Continue ;
          (* next rule application *)
          let app = Zlist.hd proof_state.apps in
          let () = assert (app.depth <= !bound) in
          let () = assert (Blist.for_all (fun (_,gd) -> gd <= !bound) app.goals) in
          (* push remaining applications *)
          let () = stack := {proof_state with apps=Zlist.tl proof_state.apps} :: !stack in
          if app.goals=[] then
            begin
              (* no subgoals left, so it must be a closed Proof.t *)
              assert (Proof.is_closed app.prf) ;
              if check app.defs then found := Some (app.prf,app.depth,app.defs);
              (* NOTE: in case not acceptable we do not pop parents as we may need to backtrack *)
              raise Continue
            end ;
          let () = assert (not (Proof.is_closed app.prf)) in
          let () = if !do_debug then
            begin
              print_endline ("Expanding node: " ^ (string_of_int (fst (Blist.hd app.goals)))) ;
              print_endline (Proof.to_string app.prf)
            end in
          if Blist.exists (fun (_,gd) -> gd = !bound) app.goals then
            begin
              (* if any of the open goals is at the current depth *)
              (* then keep for later *)
              frontier := (expand_proof_state proof_state.seq_no app rule) :: !frontier ;
              raise Continue
            end ;
          if is_closed_at proof_state.idx app.prf then
            begin
              (* last application resulted in no new open subgoals *)
              (* thus we will pop all generators of applications *)
              (* that are parents of the current one *)
              (* this is equivalent to a prolog cut over the other possible *)
              (* closed proofs of these goals *)
              stack := pop_parents proof_state.seq_no !stack
            end ;
          stack := (expand_proof_state proof_state.seq_no app rule) :: !stack
        with Continue -> ()
      done ;
      match !found with
        | None -> None
        | Some (p, d, defs) -> last_search_depth := d ; Some (p, defs)
    
  end