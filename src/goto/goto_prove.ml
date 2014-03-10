open Lib

let defs_path = ref "examples/sl.defs"
let prog_path = ref ""

module Parser = Sl_parser
module Lexer = Sl_lexer
module Prover = Prover2.Make(Goto_program.Seq)
module F = Frontend2.Make(Prover)

let sequent_of_string s =
  let lexbuf = Lexing.from_string s in
  Parser.sequent Lexer.token lexbuf

let defs_of_channel c =
  let lexbuf = Lexing.from_channel c in
  Parser.ind_def_set Lexer.token lexbuf

let program_of_channel c =
  let lexbuf = Lexing.from_channel c in
  try
    Sl_parser.program Sl_lexer.token lexbuf
  with
    | Lexer.Error msg -> print_endline msg ; assert false
    | Parser.Error -> 
      begin
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        Printf.fprintf stderr "Syntax error at line %d, column %d: token '%s'.\n%!" line cnum tok ; assert false
      end

let () = F.usage := !F.usage ^ " [-D <file>] [-P <file>]"

let () = F.speclist := !F.speclist @ [
    ("-D", Arg.Set_string defs_path, 
      ": read inductive definitions from <file>, default is " ^ !defs_path);
    ("-P", Arg.Set_string prog_path, ": prove termination of program <file>");
  ]

let () =
  Arg.parse !F.speclist (fun _ -> raise (Arg.Bad "Stray argument found.")) !F.usage ;
  if !prog_path="" then F.die "-P must be specified." ;
  let (seq, prog) = program_of_channel (open_in !prog_path) in
  Goto_program.set_program prog ; 
  Goto_rules.setup (defs_of_channel (open_in !defs_path)) seq;
  exit (F.prove_seq !Goto_rules.rules seq)
    


