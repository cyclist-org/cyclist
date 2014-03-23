#use "topfind";;
#directory "_build/src/seplog";;
#directory "_build/src/generic";;
#directory "_build/src/util";;
#directory "_build/src/goto";;
#directory "_build/src/slsat";;
#directory "_build/src/while";;
open Util;;
#install_printer Int.Set.pp;;
#install_printer Int.MSet.pp;;
#install_printer Int.FList.pp;;
#install_printer Int.Pairing.Set.pp;;
#install_printer Int.Pairing.MSet.pp;;
#install_printer Int.Pairing.FList.pp;;
#install_printer Strng.Set.pp;;
#install_printer Strng.MSet.pp;;
#install_printer Strng.FList.pp;;
#install_printer Strng.Pairing.Set.pp;;
#install_printer Strng.Pairing.MSet.pp;;
#install_printer Strng.Pairing.FList.pp;;
open Symheap;;
#install_printer Term.pp;;
#install_printer Term.Set.pp;;
#install_printer Term.pp_subst;;
#install_printer Heap.pp;;
#install_printer Form.pp;;
#install_printer Seq.pp;;
#install_printer Defs.pp;;
open Sl_rules;;
#install_printer Proof.pp;;


let parse_seq s = 
  Sl_parser.sequent Sl_lexer.token (Lexing.from_string s);;

let parse_form f = 
  Sl_parser.formula Sl_lexer.token (Lexing.from_string f);;

let parse_defs d = 
  Sl_parser.ind_def_set Sl_lexer.token (Lexing.from_string d);;

let parse_goto_program p = 
  Sl_parser.program Sl_lexer.token (Lexing.from_string p);;
  
  
  
let s = parse_seq "ls(x) * ls(y) |- ls(x) * ls(y)";;
let s' = parse_seq "ls(x) |- ls(x)";;
let f = parse_form "ls(x) * ls(y)";;
let f' = parse_form "ls(x)";;

