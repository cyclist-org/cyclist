open Lib
open Generic

open KleeneAlg

let sequent = ref ""

let () =
  Tags.alphabet := Lib.VarManager.greek_alphabet

module Prover = Prover.Make(Seq)(Rules.Infrule)
module F = Frontend.Make(Seq)(Rules.Infrule)

let () = F.usage := !F.usage ^ " [-S <string>]"

let () =
  let old_spec_thunk = !F.speclist in
  F.speclist :=
    fun () ->
      old_spec_thunk () @ [
        ("-S", Arg.Set_string sequent, ": prove the LTL sequent provided in <string>")
      ]

let () =
  let spec_list = !F.speclist () in
  Arg.parse spec_list (fun _ -> raise (Arg.Bad "Stray argument found.")) !F.usage ;
  if String.equal !sequent "" then F.die "-S must be specified." spec_list !F.usage ;
  let seq = Seq.of_string !sequent in
  let res = F.prove_seq !Rules.axioms !Rules.rules seq in
  F.print_result res ;
  F.exit res