open Temporal_program_ltl
open Temporal_rules_ltl
       
let defs_path = ref "examples/sl.defs"
let prog_path = ref ""
		    
module Prover = Prover.Make(Temporal_program_ltl.Seq)
module F = Frontend.Make(Prover)
			
let () = F.usage := !F.usage ^ " [-D <file] [-P <file>] [-IT]"
				 
let () = F.speclist := !F.speclist @ [
    ("-D", Arg.Set_string defs_path, 
      ": read inductive definitions from <file>, default is " ^ !defs_path);
    ("-P", Arg.Set_string prog_path, ": prove temporal property of program <file>");
    ("-IT", Arg.Set Sl_rules.use_invalidity_heuristic, 
     ": run invalidity heuristic during check, default is " ^ 
       (string_of_bool !Sl_rules.use_invalidity_heuristic));
  ]

let () =
  Arg.parse !F.speclist (fun _ -> raise (Arg.Bad "Stray argument found.")) !F.usage ;
  if !prog_path="" then F.die "-P must be specified." ;
  let (seq, prog, tfext) = Temporal_program_ltl.of_channel (open_in !prog_path) in
  let prog = Cmd.number prog in
  Temporal_program_ltl.set_program prog ; 
  Temporal_rules_ltl.setup (Sl_defs.of_channel (open_in !defs_path));
  Temporal_program_ltl.Seq.pp Format.std_formatter (seq, prog, tfext) ;
  exit (F.prove_seq !Temporal_rules_ltl.axioms !Temporal_rules_ltl.rules (seq, prog, tfext))

  
