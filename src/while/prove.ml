open Generic
module Prover = Prover.Make (Program.Seq)
module F = Frontend.Make (Prover)

let default_defs_path = "examples/sl.defs"

let run defs_path prog_path termination () =
  if termination then Program.termination := true;
  let seq, prog = Program.of_channel (open_in prog_path) in
  if not (Program.Cmd.is_while_prog prog) then (
    prerr_endline "cyclist: unrecognised commands in program.";
    exit Cmdliner.Cmd.Exit.cli_error);
  let prog = Program.Cmd.number prog in
  Program.set_program prog;
  Rules.setup (Seplog.Defs.of_channel (open_in defs_path));
  F.exit (F.prove_seq !Rules.axioms !Rules.rules (seq, prog))

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
  Cmd.v
    (Cmd.info "prove" ~doc:"Prove safety of a while program."
       ~exits:Frontend.exits)
    Term.(const run $ defs $ prog $ termination $ F.common_term ())
