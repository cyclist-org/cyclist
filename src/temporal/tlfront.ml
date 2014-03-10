open Lib

let cl_sequent = ref ""
let defs_path = ref "examples/sl.defs"

module Parser = Sl_parser
module Lexer = Sl_lexer
module Prover = Tlprover
(* module F = Frontend.Make(Prover)(Symheap.Seq) *)

let sequent_of_string s =
  let lexbuf = Lexing.from_string s in
  try 
     Parser.tl_sequent Lexer.token lexbuf
  with Parser.Error -> 
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let tok = Lexing.lexeme lexbuf in
    Printf.fprintf stderr 
    "At line %d, offset %d, token %s: syntax error.\n%!" line cnum tok ; 
    raise Parser.Error

let defs_of_channel c =
  let lexbuf = Lexing.from_channel c in Parser.ind_def_set Lexer.token lexbuf

let pseq s = sequent_of_string s

(* let () = Prover.setup (defs_of_channel (open_in !defs_path))  *)
    


