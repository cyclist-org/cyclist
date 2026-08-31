open Lib
open Generic
module Prover = Prover.Make (Seq)
module F = Frontend.Make (Prover)

let default_defs_path = "examples/fo.defs"

let run defs_path sequent () =
  Tags.alphabet := VarManager.arabic_digits;
  let seq = Seq.of_string sequent in
  Rules.setup (Defs.of_channel (open_in defs_path));
  F.exit (F.prove_seq !Rules.axioms !Rules.rules seq)

let cmd =
  let open Cmdliner in
  let defs =
    Arg.(
      value & opt file default_defs_path
      & info [ "D"; "defs" ] ~docv:"FILE"
          ~doc:"Read inductive definitions from $(docv).")
  in
  let sequent =
    Arg.(
      required
      & opt (some string) None
      & info [ "S"; "sequent" ] ~docv:"SEQUENT"
          ~doc:"Prove the first-order sequent provided in $(docv).")
  in
  Cmd.v
    (Cmd.info "prove" ~doc:"Prove a first-order sequent." ~exits:Frontend.exits)
    Term.(const run $ defs $ sequent $ F.common_term ())
