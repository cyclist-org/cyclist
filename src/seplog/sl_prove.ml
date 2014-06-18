open Lib

let cl_sequent = ref ""
let defs_path = ref "examples/sl.defs"

module Prover = Prover.Make(Symheap.Seq)
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
  let seq = Symheap.Seq.of_string !cl_sequent in
  Sl_rules.setup (Sl_defs.of_channel (open_in !defs_path)) ;
  exit (F.prove_seq !Sl_rules.axioms !Sl_rules.rules seq)
    


