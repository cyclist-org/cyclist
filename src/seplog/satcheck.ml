open Lib
open Generic

let default_defs_path = "examples/sl.defs"

let run defs_path show_proof timeout only_first slcomp () =
  gc_setup ();
  let slcomp_mode = Option.is_some slcomp in
  let consistency_check () =
    match slcomp with
    | Some file ->
        let defs, f = Smtlib.defs_of_channel (open_in file) in
        Basepair.form_sat defs f
    | None ->
        let defs = Defs.of_channel (open_in defs_path) in
        Basepair.satisfiable ~only_first ~output:show_proof defs
  in
  Stats.reset ();
  Stats.Gen.call ();
  let res = w_timeout consistency_check timeout in
  Stats.Gen.end_call ();
  let exit_code =
    match res with
    | None ->
        print_endline (if slcomp_mode then "unknown" else "UNKNOWN: [TIMEOUT]");
        2
    | Some false ->
        print_endline
          (if slcomp_mode then "unsat"
           else
             "UNSAT: "
             ^ (if only_first then "First" else "Some")
             ^ " *inductive rule* has an empty base.");
        1
    | Some true ->
        print_endline
          (if slcomp_mode then "sat"
           else
             "SAT: "
             ^ (if only_first then "First predicate has"
                else "All predicates have")
             ^ " a non-empty base.");
        0
  in
  if !Stats.do_statistics then Stats.gen_print ();
  exit (if slcomp_mode then 0 else exit_code)

let cmd =
  let open Cmdliner in
  let defs =
    Arg.(
      value & opt file default_defs_path
      & info [ "D"; "defs" ] ~docv:"FILE"
          ~doc:"Read inductive definitions from $(docv).")
  in
  let show_proof =
    Arg.(value & flag & info [ "p"; "show-proof" ] ~doc:"Show the proof.")
  in
  let timeout =
    Arg.(
      value & opt int 30
      & info [ "t"; "timeout" ] ~docv:"SECONDS"
          ~doc:"Timeout in seconds. 0 disables it.")
  in
  let only_first =
    Arg.(
      value & flag
      & info [ "f"; "first-only" ]
          ~doc:"Check satisfiability of the first predicate only.")
  in
  let slcomp =
    Arg.(
      value
      & opt (some file) None
      & info [ "slcomp" ] ~docv:"FILE"
          ~doc:
            "Read the problem from the SMT-LIB file $(docv) and report \
             sat/unsat/unknown, as SLCOMP expects.")
  in
  Cmd.v
    (Cmd.info "satcheck"
       ~doc:"Check that inductive predicate definitions are satisfiable."
       ~exits:
         (let open Cmd.Exit in
          [
            info ok ~doc:"if the definitions are satisfiable.";
            info 1 ~doc:"if they are not.";
            info 2 ~doc:"on timeout.";
            info cli_error ~doc:"on command line parsing errors.";
            info internal_error ~doc:"on unexpected internal errors.";
          ]))
    Term.(
      const run $ defs $ show_proof $ timeout $ only_first $ slcomp
      $ Frontend.debug_term)
