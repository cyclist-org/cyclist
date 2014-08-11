open Lib

let defs_path = ref "examples/sl.defs"
let prog_path = ref ""

module Prover = Prover.Make(Goto_program.Seq)
module F = Frontend.Make(Prover)

let () = F.usage := !F.usage ^ " [-D <file>] [-P <file>]"

let () = F.speclist := !F.speclist @ [
    ("-D", Arg.Set_string defs_path, 
      ": read inductive definitions from <file>, default is " ^ !defs_path);
    ("-P", Arg.Set_string prog_path, ": prove termination of program <file>");
  ]

let () =
  Arg.parse !F.speclist (fun _ -> raise (Arg.Bad "Stray argument found.")) !F.usage ;
  if !prog_path="" then F.die "-P must be specified." ;
  let (seq, prog) = Goto_program.of_channel (open_in !prog_path) in
  Goto_program.set_program prog ; 
  Goto_rules.setup (Sl_defs.of_channel (open_in !defs_path)) seq;
  exit (F.prove_seq !Goto_rules.axioms !Goto_rules.rules seq)
    


