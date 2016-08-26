open Asl_while_program
open Asl_while_rules

let prog_path = ref ""

module Prover = Prover.Make(Asl_while_program.Seq)
module F = Frontend.Make(Prover)

(* let () =                                                      *)
(*   let (p, c) = program_of_channel (open_in Sys.argv.(1)) in   *)
(*   let c = Cmd.number c in                                     *)
(*   Format.fprintf Format.std_formatter "%a@\n" program_pp c ;  *)
(*   Format.fprintf Format.std_formatter "\n" ;                  *)
(*   let cmd = ref c in                                          *)
(*   while !cmd<>[] do                                           *)
(*     Format.fprintf Format.std_formatter "%a@\n" pp_cmd !cmd ; *)
(*     cmd := Blist.tl !cmd                                      *)
(*   done      													 *)

let () = F.maxbound := 18

let () = F.usage := !F.usage ^ " [-D <file] [-P <file>]"

let () = F.speclist := !F.speclist @ [
    ("-P", Arg.Set_string prog_path, ": prove memory safety of program <file>");
  ]

let () =
  Arg.parse !F.speclist (fun _ -> raise (Arg.Bad "Stray argument found.")) !F.usage ;
  if !prog_path="" then F.die "-P must be specified." ;
  let (seq, prog) = Asl_while_program.of_channel (open_in !prog_path) in
  let prog = Cmd.number prog in
  Asl_while_program.set_program prog ; 
  Asl_while_rules.setup () ;
  let start = Unix.gettimeofday () in
  let code = (F.prove_seq !Asl_while_rules.axioms !Asl_while_rules.rules (seq, prog)) in
  let stop = Unix.gettimeofday () in
  Printf.printf "Execution time: %fs\n%!" (stop -. start); Printf.printf "z3 called %i times.\n" !Asl_sat.times;
  exit code
    


  