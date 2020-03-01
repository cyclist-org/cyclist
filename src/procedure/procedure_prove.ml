open Lib
open Generic
open Seplog

let defs_path = ref "examples/sl.defs"

let prog_path = ref ""

module Program = Procedure_program
module Rules = Procedure_rules
module Proc = Program.Proc
module Seq = Program.Seq
module Cmd = Program.Cmd
module GraphComponents = Graph.Components.Make (Proc.Graph)
module Prover = Prover.Make (Seq)
module F = Frontend.Make (Prover)
module Proof = Prover.Proof
module Node = Proofnode.Make (Seq)

let prove_all = ref false

let () =
  F.usage :=
    !F.usage
    ^ "[-ed/fd <int>] [-D <file>] [-d(entail|frame|invalid)] [-Lem <int>] -P \
       <file> [-T] [-all | <entry_point>*]"

let () =
  let old_spec_thunk = !F.speclist in
  F.speclist :=
    fun () ->
      old_spec_thunk ()
      @ [ ( "-ed"
          , Arg.Set_int Rules.entl_depth
          , ": maximum search depth for entailment sub-prover, default is "
            ^ string_of_int !Rules.entl_depth )
        ; ( "-fd"
          , Arg.Int Abduce.set_depth
          , ": maximum depth to unfold predicates to in frame inference, \
             default is "
            ^ string_of_int Abduce.max_depth )
        ; ( "-D"
          , Arg.Set_string defs_path
          , ": read inductive definitions from <file>, default is "
            ^ !defs_path )
        ; ( "-dentail"
          , Arg.Set Rules.show_entailment_debug
          , ": print debug messages for the entailment subprover, default is "
            ^ string_of_bool !Rules.show_entailment_debug
            ^ " (only activated when main debug output flag set)" )
        ; ( "-dframe"
          , Arg.Set Rules.show_frame_debug
          , ": print debug messages for frame inference, default is "
            ^ string_of_bool !Rules.show_frame_debug
            ^ " (only activated when main debug output flag set)" )
        ; ( "-dinvalid"
          , Arg.Set Rules.show_invalidity_debug
          , ": print debug messages for invalidity checker, default is "
            ^ string_of_bool !Rules.show_invalidity_debug
            ^ " (only activated when main debug output flag set)" )
        ; ( "-Lem"
          , Arg.Int Seplog.Rules.set_lemma_level
          , ": specify the permissiveness of the lemma application strategy \
             for proving entailments" ^ "\n"
            ^ Seplog.Rules.lemma_option_descr_str () )
        ; ( "-P"
          , Arg.Set_string prog_path
          , ": prove safety of program in <file>" )
        ; ("-T", Arg.Set Program.termination, ": also prove termination")
        ; ( "-all"
          , Arg.Set prove_all
          , ": analyse all procedures in <file>, or specify procedures to be \
             analysed (default is " ^ Program.main ^ ")" ) ]

(* disable max search depth *)
let () = F.maxbound := 0

(* Append all proofs to the same file *)
let () = F.open_file_for_append := true

let proc_proofs : Proof.t option Proc.SigMap.t ref = ref Proc.SigMap.empty

let entry_points = ref []

let add_entry_point p = entry_points := p :: !entry_points

let extract_proof prf (idx, node) =
  if Rules.is_proc_unfold_node node then
    let ((pre, cmd, post) as seq) = Node.get_seq node in
    let proc = Cmd.dest_proc_call cmd in
    let signature = (proc, (pre, post)) in
    if not (Proc.SigMap.mem signature !proc_proofs) then (
      let new_prf = Proof.extract_subproof idx prf in
      if !F.show_proof || !Stats.do_statistics then print_newline () ;
      print_endline
        ( "Extracted proof"
        ^ if !F.show_proof then ":" else " for: " ^ Seq.to_string seq ) ;
      if !F.show_proof then Proof.pp Format.std_formatter new_prf ;
      if !Stats.do_statistics then
        print_endline
          ( "Proof has "
          ^ string_of_int (Proof.size new_prf)
          ^ " nodes" ^ " and "
          ^ string_of_int (Proof.num_backlinks new_prf)
          ^ " back-links." ) ;
      proc_proofs := Proc.SigMap.add signature (Some new_prf) !proc_proofs )

let prove_seq ((pre, cmd, post) as seq) =
  assert (Cmd.is_proc_call cmd) ;
  assert (Cmd.is_empty (Cmd.get_cont cmd)) ;
  let proc = Cmd.dest_proc_call cmd in
  let signature = (proc, (pre, post)) in
  if not (Proc.SigMap.mem signature !proc_proofs) then (
    Lib.debug (fun () -> "Beginning search for proof of: " ^ Seq.to_string seq) ;
    if !F.show_proof || !Stats.do_statistics then print_newline () ;
    match F.prove_seq !Rules.axioms !Rules.rules seq with
    | TIMEOUT | NOT_FOUND ->
        proc_proofs := Proc.SigMap.add signature None !proc_proofs
    | SUCCESS prf ->
        proc_proofs := Proc.SigMap.add signature (Some prf) !proc_proofs ;
        Blist.iter (extract_proof prf) (Proof.to_list prf) )

let prove_scc ps = Blist.iter prove_seq (Blist.bind Proc.get_seqs ps)

module Timer = Stats.TimeStats ()

let () =
  let spec_list = !F.speclist () in
  Arg.parse spec_list add_entry_point !F.usage ;
  if String.equal !prog_path "" then
    F.die "-P must be specified." spec_list !F.usage ;
  let fields, procs = Program.of_channel (open_in !prog_path) in
  let procs = Blist.map Proc.number_cmds procs in
  let defs = Defs.of_channel (open_in !defs_path) in
  (* TODO: Check well-formedness of the program: *)
  (*   Do all the predicates in the pre/post annotations have the correct arity? *)
  (*   If not While_program.well_formed defs prog then F.die !While_program.error_msg *)
  Program.set_program (fields, procs) ;
  Rules.setup (defs, procs, proc_proofs) ;
  let proc_names = Blist.map Proc.get_name procs in
  let entry_points =
    if !prove_all then proc_names
    else if Blist.is_empty !entry_points then [Program.main]
    else !entry_points
  in
  Blist.iter
    (fun p ->
      try ignore (Program.get_proc p) with Not_found ->
        F.die (p ^ " procedure not found!") spec_list !F.usage )
    entry_points ;
  let reachable = Program.get_reachable entry_points in
  Blist.iter
    (fun p ->
      if not (Proc.Graph.mem_vertex reachable p) then
        print_endline (Proc.get_name p ^ " is not reachable - ignoring.") )
    procs ;
  let sccs = GraphComponents.scc_list reachable in
  Timer.call () ;
  Blist.iter prove_scc sccs ;
  Timer.end_call () ;
  if !Stats.do_statistics then
    Printf.printf "\nTotal time taken: %.0f ms\n" (1000.0 *. !Timer.cpu_time) ;
  let res =
    Blist.for_all
      (fun p ->
        let proc = Program.get_proc p in
        let head = (Proc.get_name proc, Proc.get_params proc) in
        try
          Blist.for_all
            (fun (pre, _, post) ->
              Option.is_some
                (Proc.SigMap.find (head, (pre, post)) !proc_proofs) )
            (Proc.get_seqs proc)
        with Not_found -> false )
      entry_points
  in
  if res then exit 0 else exit 1
