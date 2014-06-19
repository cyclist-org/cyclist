#use "topfind";;
#directory "_build/src/seplog";;
#directory "_build/src/generic";;
#directory "_build/src/util";;
#directory "_build/src/goto";;
#directory "_build/src/slsat";;
#directory "_build/src/while";;
#directory "_build/src/mparser";;
open Util;;
#install_printer Format.pp_print_string;;
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
#install_printer Sl_form.pp;;
#install_printer Sl_seq.pp;;
#install_printer Defs.pp;;
open Goto_rules;;
#install_printer Proof.pp;;

open Lib;;

(* let parse_seq s =                                                                *)
(*   Sl_parser.sequent Sl_lexer.token (Lexing.from_string s);;                      *)

(* let parse_form f =                                                               *)
(*   Sl_parser.formula Sl_lexer.token (Lexing.from_string f);;                      *)

(* let parse_defs d =                                                               *)
(*   Sl_parser.ind_def_set Sl_lexer.token (Lexing.from_string d);;                  *)

(* let parse_goto_program p =                                                       *)
(*   Sl_parser.program Sl_lexer.token (Lexing.from_string p);;                      *)
  
(* let defs_of_file f = parse_defs (string_of_file f)                               *)
  
(* let f = parse_form "w'!=nil * w'->j' * ls_1(y,w') * ls_2(j',nil) * ls_3(x,w')";; *)
(* let f' = parse_form "y=z * nil!=z * z->w' * ls_2(w',nil) * ls_3(x,z)";;          *)
(* let defs = defs_of_file "examples/sl.defs"                                       *)
(* let prf = Proof.mk (f',1);;                                                      *)
(* let apps = Rule.choice (Blist.map fold defs) 0 prf;;                             *)

open MParser;;

let f = parse_string Symheap.Heap.parse "x!=y" () ;;
