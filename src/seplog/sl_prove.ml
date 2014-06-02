open Lib

let cl_sequent = ref ""
let defs_path = ref "examples/sl.defs"

module Parser = Sl_parser
module Lexer = Sl_lexer
module Prover = Prover.Make(Symheap.Seq)
module F = Frontend.Make(Prover)

let sequent_of_string s =
  let lexbuf = Lexing.from_string s in
  try 
    Parser.sequent Lexer.token lexbuf
  with
    | Lexer.Error msg -> print_endline msg ; assert false
    | Parser.Error -> 
      (* Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf) ; assert false *)
      begin
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        Printf.fprintf stderr "Syntax error at line %d, column %d: token '%s'.\n%!" line cnum tok ; assert false
      end
      
let defs_of_channel c =
  let lexbuf = Lexing.from_channel c in
  try 
    Parser.ind_def_set Lexer.token lexbuf
  with
    | Lexer.Error msg -> print_endline msg ; assert false
    | Parser.Error -> Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf) ; assert false

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
  let seq = sequent_of_string !cl_sequent in
  Sl_rules.setup (defs_of_channel (open_in !defs_path)) ;
  exit (F.prove_seq !Sl_rules.axioms !Sl_rules.rules seq)
    


