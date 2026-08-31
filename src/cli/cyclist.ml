(* The single entry point for every prover in the distribution. Each command
   module lives next to the library it drives; this module only assembles them
   into a command tree. *)

open Cmdliner

let group name ~doc cmds = Cmd.group (Cmd.info name ~doc) cmds

let cmds =
  [
    group "fo" ~doc:"First-order logic with inductive definitions."
      [ Fol.Prove.cmd ];
    group "ltl" ~doc:"Linear temporal logic." [ Ltl.Prove.cmd ];
    group "sl" ~doc:"Separation logic."
      [
        Seplog.Prove.cmd;
        Seplog.Disprove.cmd;
        Seplog.Modelcheck.cmd;
        Seplog.Satcheck.cmd;
        Seplog.Satexpgen.cmd;
      ];
    group "while" ~doc:"A while language over separation logic."
      [ While.Prove.cmd; While.Abduce.cmd ];
    group "asl-while" ~doc:"A while language over array separation logic."
      [ Asl_while.Prove.cmd ];
    group "proc" ~doc:"A while language with procedures."
      [ Procedure.Prove.cmd ];
    Generic.Checkproof.cmd;
  ]

let main =
  let doc = "A framework for building cyclic theorem provers" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) bundles the Cyclist provers. Each logic or language has its \
         own group of subcommands; see $(tname) $(i,COMMAND) --help for the \
         options of a particular prover.";
      `S Manpage.s_bugs;
      `P "Report issues at https://github.com/cyclist-org/cyclist/issues";
    ]
  in
  let version =
    (* Reports the package version once the project sets one (in dune-project,
       or via `dune subst` on a release); a plain dev checkout has none. *)
    match Build_info.V1.version () with
    | Some v -> Build_info.V1.Version.to_string v
    | None -> "n/a"
  in
  Cmd.group (Cmd.info "cyclist" ~version ~doc ~man) cmds

(* Proof pretty-printing is laid out to the width of the terminal, when there
   is one. Guarding on TERM keeps `tput` from writing an error to stderr when
   the output is a pipe or a log, which is how the benchmarks run. *)
let set_margin_from_terminal () =
  match Sys.getenv_opt "TERM" with
  | None | Some "" | Some "dumb" -> ()
  | Some _ ->
      let cols = Sys.command "exit $(tput cols 2>/dev/null)" in
      if cols > 1 then Format.set_margin cols

let () =
  set_margin_from_terminal ();
  exit (Cmd.eval main)
