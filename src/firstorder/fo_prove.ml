open Lib

let cl_sequent = ref ""
let defs_path = ref "examples/fo.defs"

module Parser = Fo_parser
module Lexer = Fo_lexer
module Prover = Prover.Make(Firstorder.Seq)
module F = Frontend.Make(Prover)

let sequent_of_string s =
  let lexbuf = Lexing.from_string s in
  Parser.sequent Lexer.token lexbuf

let defs_of_channel c =
  let lexbuf = Lexing.from_channel c in
  Parser.ind_def_set Lexer.token lexbuf

let () = F.usage := !F.usage ^ " [-D <file>] [-S <string>]"

let () = F.speclist := !F.speclist @ [
    ("-D", Arg.Set_string defs_path, 
      ": read inductive definitions from <file>, default is " ^ !defs_path);
    ("-S", Arg.Set_string cl_sequent, ": prove the FO sequent provided in <string>")
  ]

let () =
  Arg.parse !F.speclist (fun _ -> raise (Arg.Bad "Stray argument found.")) !F.usage ;
  if !cl_sequent="" then F.die "-S must be specified." ;
  let seq = sequent_of_string !cl_sequent in
  Fo_rules.setup (defs_of_channel (open_in !defs_path)) ;
  exit (F.prove_seq !Fo_rules.axioms !Fo_rules.rules seq)
    


