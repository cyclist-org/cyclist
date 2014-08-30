open Lib

let cl_sequent = ref ""
let defs_path = ref "examples/sl.defs"

module Prover = Prover.Make(Sl_seq)
module F = Frontend.Make(Prover)

let () = F.usage := !F.usage ^ " [-D <file>] [-S <string>]"

let () = F.speclist := !F.speclist @ [
    ("-D", Arg.Set_string defs_path, 
      ": read inductive definitions from <file>, default is " ^ !defs_path);
    ("-S", Arg.Set_string cl_sequent, ": prove the SL sequent provided in <string>");
  ]

let () =
  gc_setup () ;
  Arg.parse !F.speclist (fun _ -> raise (Arg.Bad "Stray argument found.")) !F.usage ;
  if !cl_sequent="" then F.die "-S must be specified." ;
  let seq = Sl_seq.of_string !cl_sequent in
  let defs = Sl_defs.of_channel (open_in !defs_path) in
  Sl_rules.setup defs ;
  if Sl_seq.invalid defs seq then
    begin
      print_endline ("NOT proved: " ^ (Sl_seq.to_string seq) ^ " [invalid]") ; 
      exit 255
    end ;    
  exit (F.prove_seq !Sl_rules.axioms !Sl_rules.rules seq)
    


