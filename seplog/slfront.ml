open Lib

let cl_sequent = ref ""
let defs_path = ref "examples/sl.defs"

module Parser = Slparser
module Lexer = Sllexer
module Prover = Slprover
module F = Frontend.Make(Prover)(Symheap.Seq)

let sequent_of_string s =
  let lexbuf = Lexing.from_string s in Parser.sequent Lexer.token lexbuf

let defs_of_channel c =
  let lexbuf = Lexing.from_channel c in Parser.ind_def_set Lexer.token lexbuf

let pseq s = sequent_of_string s

let () = Prover.setup (defs_of_channel (open_in !defs_path)) 
    


