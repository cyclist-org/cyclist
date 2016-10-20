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
let () = F.usage := !F.usage ^ " [-D <file>] [-Lem <int>] -P <file> [-T]"

let () = 
  let old_spec_thunk = !F.speclist in
  F.speclist := 
    (fun () -> old_spec_thunk() @ [
      ("-D", Arg.Set_string defs_path, 
        ": read inductive definitions from <file>, default is " ^ !defs_path);
      ("-Lem", Arg.Int Sl_rules.set_lemma_level,
        ": specify the permissiveness of the lemma application strategy for proving entailments" ^ "\n" ^ 
        Sl_rules.lemma_option_descr_str());
      ("-P", Arg.Set_string prog_path, ": prove safety of program <file>");
      ("-T", Arg.Set termination, ": also prove termination");
    ])
  
(* disable max search depth *)
let () = F.maxbound := 0

let () =
  let spec_list = !F.speclist() in
  Arg.parse spec_list (fun _ -> raise (Arg.Bad "Stray argument found.")) !F.usage ;
  if !prog_path="" then F.die "-P must be specified." spec_list !F.usage ;
  let ((pre, cmd, post), procs) = Extended_while_program.of_channel (open_in !prog_path) in
  let main = (pre, Cmd.number cmd, post) in
  let procs = Blist.map (fun (id, params, specs, body) -> (id, params, specs, Cmd.number body)) procs in
	let defs = Sl_defs.of_channel (open_in !defs_path) in
	(* TODO: Check well-formedness of the program: *)
	(*   Do all the predicates in the pre/post annotations have the correct arity? *)
	(*   If not While_program.well_formed defs prog then F.die !While_program.error_msg *)
  Extended_while_program.set_program (main, procs); 
  Extended_while_rules.setup (defs, procs) ;
  exit (F.prove_seq !Extended_while_rules.axioms !Extended_while_rules.rules main)
    


  