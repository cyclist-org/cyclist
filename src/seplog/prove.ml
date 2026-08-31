open Lib
open Generic
module Prover = Prover.Make (Seq)
module F = Frontend.Make (Prover)

let default_defs_path = "examples/sl.defs"

(* [Rules.set_lemma_level] raises on anything outside 0..3, so let Cmdliner
   reject it first and report it as an ordinary command line error. *)
let level = Cmdliner.Arg.enum [ ("0", 0); ("1", 1); ("2", 2); ("3", 3) ]

let run defs_path parse_null_as_emp invalidity_check slcomp cl_sequent () () =
  gc_setup ();
  let slcomp_mode = Option.is_some slcomp in
  let seq, defs =
    match (slcomp, cl_sequent) with
    | Some file, _ -> Smtlib.of_channel (open_in file)
    | None, Some s ->
        ( Seq.of_string ~null_is_emp:parse_null_as_emp s,
          Defs.of_channel (open_in defs_path) )
    | None, None ->
        prerr_endline "cyclist: one of --sequent or --slcomp must be specified.";
        exit Cmdliner.Cmd.Exit.cli_error
  in
  Rules.setup defs;
  let res =
    F.gather_stats (fun () ->
        if invalidity_check && Invalid.check defs seq then None
        else Some (F.idfs !Rules.axioms !Rules.rules seq))
  in
  match res with
  | Some None ->
      print_endline
        (if slcomp_mode then "sat"
         else "NOT proved: " ^ Seq.to_string seq ^ " [invalid]");
      exit 255
  | _ ->
      let res = Option.flatten res in
      if slcomp_mode then (
        (match res with
        | Some (Some _) -> print_endline "unsat"
        | _ -> print_endline "unknown");
        exit 0)
      else F.exit (F.process_result true seq res)

let cmd =
  let open Cmdliner in
  let defs =
    Arg.(
      value & opt file default_defs_path
      & info [ "D"; "defs" ] ~docv:"FILE"
          ~doc:"Read inductive definitions from $(docv).")
  in
  let emp =
    Arg.(
      value & flag
      & info [ "emp" ]
          ~doc:
            "Parse the empty string as the formula $(b,emp) rather than \
             $(b,False).")
  in
  let backlink_select =
    Arg.(
      value
      & opt (some level) None
      & info [ "backlink-select" ] ~docv:"INT"
          ~doc:
            "Which proof nodes are considered for backlinks: 0 for all proof \
             nodes (the default), 1 for all closed proof nodes, 2 for all \
             ancestor proof nodes, 3 for all syntactically equal proof nodes.")
  in
  let lemma_level =
    Arg.(
      value
      & opt (some level) None
      & info [ "lemma-level" ] ~docv:"INT"
          ~doc:
            "How permissive the lemma application strategy is: 0 to apply no \
             lemmas, 1 to only apply lemmas containing predicate instances \
             (the default), 2 to only apply lemmas with non-empty spatial \
             components, 3 to attempt all applicable lemmas.")
  in
  let invalidity_check =
    Arg.(
      value & flag
      & info [ "invalidity-check" ]
          ~doc:"Run the invalidity heuristic before the search.")
  in
  let invalidity_during_search =
    Arg.(
      value & flag
      & info
          [ "invalidity-during-search" ]
          ~doc:"Run the invalidity heuristic during the search.")
  in
  let partition_strengthening =
    Arg.(
      value & flag
      & info
          [ "partition-strengthening" ]
          ~doc:"Use partition strengthening in the invalidity heuristic.")
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
  let sequent =
    Arg.(
      value
      & opt (some string) None
      & info [ "S"; "sequent" ] ~docv:"SEQUENT"
          ~doc:"Prove the separation logic sequent provided in $(docv).")
  in
  (* These four only mutate configuration held elsewhere, so they are applied
     as the term is evaluated rather than passed on to [run]. *)
  let side_effects =
    let apply bl lem it ip =
      Option.iter Rules.set_default_select_f bl;
      Option.iter Rules.set_lemma_level lem;
      if it then Rules.use_invalidity_heuristic := true;
      if ip then Invalid.partition_strengthening := true
    in
    Term.(
      const apply $ backlink_select $ lemma_level $ invalidity_during_search
      $ partition_strengthening)
  in
  Cmd.v
    (Cmd.info "prove" ~doc:"Prove a separation logic entailment."
       ~exits:
         (Frontend.exits
         @ [ Cmd.Exit.info 255 ~doc:"if the sequent was shown to be invalid." ]
         ))
    Term.(
      const run $ defs $ emp $ invalidity_check $ slcomp $ sequent
      $ F.common_term ~max_depth:0 ()
      $ side_effects)
