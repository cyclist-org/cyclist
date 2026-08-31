open Lib
open Generic
module Prover = Prover.Make (Seq)
module F = Frontend.Make (Prover)

let run sequent () =
  Tags.alphabet := VarManager.arabic_digits;
  let seq = Seq.of_string sequent in
  F.exit (F.prove_seq !Rules.axioms !Rules.rules seq)

let cmd =
  let open Cmdliner in
  let sequent =
    Arg.(
      required
      & opt (some string) None
      & info [ "S"; "sequent" ] ~docv:"SEQUENT"
          ~doc:"Prove the LTL sequent provided in $(docv).")
  in
  Cmd.v
    (Cmd.info "prove" ~doc:"Prove an LTL sequent." ~exits:Frontend.exits)
    Term.(const run $ sequent $ F.common_term ())
