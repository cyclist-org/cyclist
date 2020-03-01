open Lib
open Generic
open Seplog

open While
open   Program
open   Rules

let defs_path = ref "examples/sl.defs"

let prog_path = ref ""

module Prover = Prover.Make (Seq)
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
        ; ("-T", Arg.Set termination, ": also prove termination")
        ]

let () =
  let spec_list = !F.speclist () in
  Arg.parse spec_list
    (fun _ -> raise (Arg.Bad "Stray argument found."))
    !F.usage ;
  if String.equal !prog_path "" then
    F.die "-P must be specified." spec_list !F.usage ;
  let seq, prog = of_channel (open_in !prog_path) in
  if not (Cmd.is_while_prog prog) then
    F.die "Unrecognised commands in program!" spec_list !F.usage ;
  let prog = Cmd.number prog in
  set_program prog ;
  setup (Defs.of_channel (open_in !defs_path)) ;
  F.exit (F.prove_seq !axioms !rules (seq, prog))
