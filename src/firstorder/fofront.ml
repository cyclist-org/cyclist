open Lib

let cl_sequent = ref ""
let defs_path = ref "examples/fo.defs"

module Parser = Foparser
module Lexer = Folexer
module Prover = Foprover
module F = Frontend.Make(Prover)(Firstorder.Seq)

let sequent_of_string s =
  let lexbuf = Lexing.from_string s in
  Parser.sequent Lexer.token lexbuf

let defs_of_channel c =
  let lexbuf = Lexing.from_channel c in
  Parser.ind_def_set Lexer.token lexbuf

let pseq = sequent_of_string

let () = Prover.setup (defs_of_channel (open_in !defs_path)) ;
    


