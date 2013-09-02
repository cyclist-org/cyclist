open Lib

let defs_path = ref "examples/sl.defs"
let prog_path = ref ""

module Parser = Slparser
module Lexer = Sllexer
module Prover = Tlprover
module F = Frontend.Make(Prover)(Tempform.Seq)

let sequent_of_string s =
  let lexbuf = Lexing.from_string s in
  Parser.sequent Lexer.token lexbuf

let defs_of_channel c =
  let lexbuf = Lexing.from_channel c in
  Parser.ind_def_set Lexer.token lexbuf

let program_of_channel c =
  let lexbuf = Lexing.from_channel c in
  Slparser.tl_program Sllexer.token lexbuf

let () = F.usage := !F.usage ^ " [-D <file] [-P <file>]"

let () = F.speclist := !F.speclist @ [
    ("-D", Arg.Set_string defs_path, 
      ": read inductive definitions from <file>, default is " ^ !defs_path);
    ("-P", Arg.Set_string prog_path, ": prove termination of program <file>");
  ]

let () =
  Arg.parse !F.speclist (fun _ -> raise (Arg.Bad "Stray argument found.")) !F.usage ;
  if !prog_path="" then F.die "-P must be specified." ;
  let (seq, prog) = program_of_channel (open_in !prog_path) in
  Program.set_program prog ; 
  Prover.setup (defs_of_channel (open_in !defs_path)) ;
  exit (F.prove_seq (Tempform.Seq.norm seq))
    


