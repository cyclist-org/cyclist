open Lib
open Generic
open Seplog

open While
open   While_program

open While_rules

let defs_path = ref "examples/sl.defs"

let prog_path = ref ""

module Prover = Prover.Make (While_program.Seq)
module F = Frontend.Make (Prover)

(* let () =                                                      *)
(*   let (p, c) = program_of_channel (open_in Sys.argv.(1)) in   *)
(*   let c = Cmd.number c in                                     *)
(*   Format.fprintf Format.std_formatter "%a@\n" program_pp c ;  *)
(*   Format.fprintf Format.std_formatter "\n" ;                  *)
(*   let cmd = ref c in                                          *)
(*   while !cmd<>[] do                                           *)
(*     Format.fprintf Format.std_formatter "%a@\n" pp_cmd !cmd ; *)
(*     cmd := Blist.tl !cmd                                       *)
(*   done                                                        *)
let () = F.usage := !F.usage ^ " [-D <file>] -P <file> [-T]"

let () =
  let old_spec_thunk = !F.speclist in
  F.speclist :=
    fun () ->
      old_spec_thunk ()
      @ [ ( "-D"
          , Arg.Set_string defs_path
          , ": read inductive definitions from <file>, default is "
            ^ !defs_path )
        ; ("-P", Arg.Set_string prog_path, ": prove safety of program <file>")
        ; ("-T", Arg.Set While_program.termination, ": also prove termination")
        ]

let () =
  let spec_list = !F.speclist () in
  Arg.parse spec_list
    (fun _ -> raise (Arg.Bad "Stray argument found."))
    !F.usage ;
  if String.equal !prog_path "" then
    F.die "-P must be specified." spec_list !F.usage ;
  let seq, prog = While_program.of_channel (open_in !prog_path) in
  if not (Cmd.is_while_prog prog) then
    F.die "Unrecognised commands in program!" spec_list !F.usage ;
  let prog = Cmd.number prog in
  While_program.set_program prog ;
  While_rules.setup (Defs.of_channel (open_in !defs_path)) ;
  F.exit (F.prove_seq !While_rules.axioms !While_rules.rules (seq, prog))
