open Extended_while_program
open Extended_while_rules

let defs_path = ref "examples/sl.defs"
let prog_path = ref ""

module Prover = Prover.Make(Extended_while_program.Seq)
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
(*   done                                                        *)
let () = F.usage := !F.usage ^ " [-D <file>] -P <file>"

let () = F.speclist := !F.speclist @ [
    ("-D", Arg.Set_string defs_path, 
      ": read inductive definitions from <file>, default is " ^ !defs_path);
    ("-P", Arg.Set_string prog_path, ": prove termination of program <file>");
    ("-T", Arg.Set termination, ": also prove termination, default is " ^ 
          (string_of_bool !termination));
  ]

let () =
  Arg.parse !F.speclist (fun _ -> raise (Arg.Bad "Stray argument found.")) !F.usage ;
  if !prog_path="" then F.die "-P must be specified." ;
  let (pre, prog, post) = Extended_while_program.of_channel (open_in !prog_path) in
  let prog = Cmd.number prog in
	let defs = Sl_defs.of_channel (open_in !defs_path) in
	(* Check well-formedness of the program: *)
	(* Do all the predicates in the pre/post annotations have the correct arity? *)
	(* Do all procedure declarations contain distinct formal parameters? *)
	(* if not While_program.well_formed defs prog then F.die !While_program.error_msg *)
  Extended_while_program.set_program prog ; 
  Extended_while_rules.setup defs ;
  exit (F.prove_seq !Extended_while_rules.axioms !Extended_while_rules.rules (pre, prog, post))
    


  