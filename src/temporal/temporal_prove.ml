open Temporal_program
open Temporal_rules

let defs_path = ref "examples/sl.defs"
let prog_path = ref ""

module Prover = Prover.Make(Temporal_program.Seq)
module F = Frontend.Make(Prover)

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
let () = F.usage := !F.usage ^ " [-D <file] [-P <file>]"

let () = F.speclist := !F.speclist @ [
    ("-D", Arg.Set_string defs_path, 
      ": read inductive definitions from <file>, default is " ^ !defs_path);
    ("-P", Arg.Set_string prog_path, ": prove termination of program <file>");
    ("-T", Arg.Set Temporal_program.termination, ": also prove termination, default is " ^ 
          (string_of_bool !Temporal_program.termination));
  ]

let () =
  Arg.parse !F.speclist (fun _ -> raise (Arg.Bad "Stray argument found.")) !F.usage ;
  if !prog_path="" then F.die "-P must be specified." ;
  let (seq, prog, tfext) = Temporal_program.of_channel (open_in !prog_path) in
  let prog = Cmd.number prog in
  Temporal_program.set_program prog ; 
  Temporal_rules.setup (Sl_defs.of_channel (open_in !defs_path)) ;
  Temporal_program.Seq.pp Format.std_formatter (seq, prog, tfext) ;
  exit (F.prove_seq !Temporal_rules.axioms !Temporal_rules.rules (seq, prog, tfext)) 
    


  
