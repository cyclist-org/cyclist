open Lib
open Generic

open Fol

let cl_sequent = ref ""
let defs_path = ref "examples/fo.defs"

let () =
  Tags.alphabet := VarManager.arabic_digits

module Prover = Prover.Make(Seq)
module F = Frontend.Make(Prover)

let () = F.usage := !F.usage ^ " [-D <file>] [-S <string>]"

let () = 
  let old_spec_thunk = !F.speclist in
  F.speclist := 
    fun () ->
      old_spec_thunk () @ [
        ("-D", Arg.Set_string defs_path, 
          ": read inductive definitions from <file>, default is " ^ !defs_path);
        ("-S", Arg.Set_string cl_sequent, ": prove the FO sequent provided in <string>")
      ]

let () =
  let spec_list = !F.speclist () in
  Arg.parse spec_list (fun _ -> raise (Arg.Bad "Stray argument found.")) !F.usage ;
  if String.equal !cl_sequent "" then F.die "-S must be specified." spec_list !F.usage ;
  let seq = Seq.of_string !cl_sequent in
  Rules.setup (Defs.of_channel (open_in !defs_path)) ;
  F.exit (F.prove_seq !Rules.axioms !Rules.rules seq)