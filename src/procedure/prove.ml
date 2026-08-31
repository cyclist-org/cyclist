open Lib
open Generic
open Program
module GraphComponents = Graph.Components.Make (Proc.Graph)
module Prover = Prover.Make (Seq)
module F = Frontend.Make (Prover)
module Proof = Prover.Proof
module Node = Proofnode.Make (Seq)

let default_defs_path = "examples/sl.defs"
let proc_proofs : Proof.t option Proc.SigMap.t ref = ref Proc.SigMap.empty

let extract_proof prf (idx, node) =
  if Rules.is_proc_unfold_node node then
    let ((pre, cmd, post) as seq) = Node.get_seq node in
    let proc = Cmd.dest_proc_call cmd in
    let signature = (proc, (pre, post)) in
    if not (Proc.SigMap.mem signature !proc_proofs) then (
      let new_prf = Proof.extract_subproof idx prf in
      if !F.show_proof || !Stats.do_statistics then print_newline ();
      print_endline
        ("Extracted proof"
        ^ if !F.show_proof then ":" else " for: " ^ Seq.to_string seq);
      if !F.show_proof then Proof.pp Format.std_formatter new_prf;
      if !Stats.do_statistics then
        print_endline
          ("Proof has "
          ^ string_of_int (Proof.size new_prf)
          ^ " nodes" ^ " and "
          ^ string_of_int (Proof.num_backlinks new_prf)
          ^ " back-links.");
      proc_proofs := Proc.SigMap.add signature (Some new_prf) !proc_proofs)

let prove_seq ((pre, cmd, post) as seq) =
  assert (Cmd.is_proc_call cmd);
  assert (Cmd.is_empty (Cmd.get_cont cmd));
  let proc = Cmd.dest_proc_call cmd in
  let signature = (proc, (pre, post)) in
  if not (Proc.SigMap.mem signature !proc_proofs) then (
    Lib.debug (fun () -> "Beginning search for proof of: " ^ Seq.to_string seq);
    if !F.show_proof || !Stats.do_statistics then print_newline ();
    match F.prove_seq !Rules.axioms !Rules.rules seq with
    | TIMEOUT | NOT_FOUND ->
        proc_proofs := Proc.SigMap.add signature None !proc_proofs
    | SUCCESS prf ->
        proc_proofs := Proc.SigMap.add signature (Some prf) !proc_proofs;
        Blist.iter (extract_proof prf) (Proof.to_list prf))

let prove_scc ps = Blist.iter prove_seq (Blist.bind Proc.get_seqs ps)

module Timer = Stats.TimeStats (struct end)

let die msg =
  prerr_endline ("cyclist: " ^ msg);
  exit Cmdliner.Cmd.Exit.cli_error

let run defs_path prog_path termination prove_all cl_entry_points () () =
  if termination then Program.termination := true;
  let fields, procs = Program.of_channel (open_in prog_path) in
  let procs = Blist.map Proc.number_cmds procs in
  let defs = Seplog.Defs.of_channel (open_in defs_path) in
  (* TODO: Check well-formedness of the program: *)
  (*   Do all the predicates in the pre/post annotations have the correct arity? *)
  Program.set_program (fields, procs);
  Rules.setup (defs, procs, proc_proofs);
  let proc_names = Blist.map Proc.get_name procs in
  let entry_points =
    if prove_all then proc_names
    else if Blist.is_empty cl_entry_points then [ Program.main ]
    else cl_entry_points
  in
  Blist.iter
    (fun p ->
      try ignore (Program.get_proc p)
      with Not_found -> die (p ^ " procedure not found!"))
    entry_points;
  let reachable = Program.get_reachable entry_points in
  Blist.iter
    (fun p ->
      if not (Proc.Graph.mem_vertex reachable p) then
        print_endline (Proc.get_name p ^ " is not reachable - ignoring."))
    procs;
  let sccs = GraphComponents.scc_list reachable in
  Timer.call ();
  Blist.iter prove_scc sccs;
  Timer.end_call ();
  if !Stats.do_statistics then
    Printf.printf "\nTotal time taken: %.0f ms\n" (1000.0 *. !Timer.cpu_time);
  let res =
    Blist.for_all
      (fun p ->
        let proc = Program.get_proc p in
        let head = (Proc.get_name proc, Proc.get_params proc) in
        try
          Blist.for_all
            (fun (pre, _, post) ->
              Option.is_some (Proc.SigMap.find (head, (pre, post)) !proc_proofs))
            (Proc.get_seqs proc)
        with Not_found -> false)
      entry_points
  in
  if res then exit 0 else exit 1

let cmd =
  let open Cmdliner in
  let defs =
    Arg.(
      value & opt file default_defs_path
      & info [ "D"; "defs" ] ~docv:"FILE"
          ~doc:"Read inductive definitions from $(docv).")
  in
  let prog =
    Arg.(
      required
      & opt (some file) None
      & info [ "P"; "program" ] ~docv:"FILE"
          ~doc:"Prove safety of the program in $(docv).")
  in
  let termination =
    Arg.(
      value & flag & info [ "T"; "termination" ] ~doc:"Also prove termination.")
  in
  let prove_all =
    Arg.(
      value & flag
      & info [ "all" ] ~doc:"Analyse every procedure in the program file.")
  in
  let entry_points =
    Arg.(
      value & pos_all string []
      & info [] ~docv:"PROC"
          ~doc:
            ("The procedures to analyse. Defaults to " ^ Program.main
           ^ "; see also $(b,--all)."))
  in
  let entl_depth =
    Arg.(
      value
      & opt (some int) None
      & info [ "entl-depth" ] ~docv:"INT"
          ~doc:"Maximum search depth for the entailment sub-prover.")
  in
  let frame_depth =
    Arg.(
      value
      & opt (some int) None
      & info [ "frame-depth" ] ~docv:"INT"
          ~doc:"Maximum depth to unfold predicates to during frame inference.")
  in
  let lemma_level =
    Arg.(
      value
      & opt (some (enum [ ("0", 0); ("1", 1); ("2", 2); ("3", 3) ])) None
      & info [ "lemma-level" ] ~docv:"INT"
          ~doc:
            "How permissive the lemma application strategy is when proving \
             entailments: 0 to apply no lemmas, 1 to only apply lemmas \
             containing predicate instances (the default), 2 to only apply \
             lemmas with non-empty spatial components, 3 to attempt all \
             applicable lemmas.")
  in
  let debug_entailment =
    Arg.(
      value & flag
      & info [ "debug-entailment" ]
          ~doc:
            "Print debug messages for the entailment sub-prover (only when \
             $(b,--debug) is also set).")
  in
  let debug_frame =
    Arg.(
      value & flag
      & info [ "debug-frame" ]
          ~doc:
            "Print debug messages for frame inference (only when $(b,--debug) \
             is also set).")
  in
  let debug_invalidity =
    Arg.(
      value & flag
      & info [ "debug-invalidity" ]
          ~doc:
            "Print debug messages for the invalidity checker (only when \
             $(b,--debug) is also set).")
  in
  (* These only mutate configuration held elsewhere, so they are applied as the
     term is evaluated rather than passed on to [run]. *)
  let side_effects =
    let apply ed fd lem de df di =
      Option.iter (fun n -> Rules.entl_depth := n) ed;
      Option.iter Seplog.Abduce.set_depth fd;
      Option.iter Seplog.Rules.set_lemma_level lem;
      if de then Rules.show_entailment_debug := true;
      if df then Rules.show_frame_debug := true;
      if di then Rules.show_invalidity_debug := true
    in
    Term.(
      const apply $ entl_depth $ frame_depth $ lemma_level $ debug_entailment
      $ debug_frame $ debug_invalidity)
  in
  Cmd.v
    (Cmd.info "prove" ~doc:"Prove safety of a while program with procedures."
       ~exits:
         (let open Cmd.Exit in
          [
            info ok ~doc:"if every entry point was proved.";
            info 1 ~doc:"if any entry point was not proved.";
            info cli_error ~doc:"on command line parsing errors.";
            info internal_error ~doc:"on unexpected internal errors.";
          ]))
    Term.(
      const run $ defs $ prog $ termination $ prove_all $ entry_points
      (* max search depth disabled *)
      $ F.common_term ~max_depth:0 ()
      $ side_effects)
