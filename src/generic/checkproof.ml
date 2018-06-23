open MParser
open MParser_PCRE

open Lib
open Parsers
open Soundcheck

let () = do_debug := true

let usage = "usage: " ^ Sys.argv.(0) ^ " [abstract proof]"

let parse_proof st =
  let parse_node st =
    ( Tokens.parens (
      Tokens.integer >>= (fun id ->
      spaces >> Tokens.comma >> spaces >>
      (parse_list Tokens.integer) >>= (fun tags ->
      spaces >> Tokens.comma >> spaces >>
      (parse_list (
        Tokens.parens (
        Tokens.integer >>= (fun target ->
        spaces >> Tokens.comma >> spaces >>
        (parse_list
          (parse_pair Tokens.integer Tokens.integer)) >>= (fun all_pairs ->
        spaces >> Tokens.comma >> spaces >>
        (parse_list (parse_pair Tokens.integer Tokens.integer)) |>>
        (fun prog_pairs -> (target, all_pairs, prog_pairs))))))) |>>
      (fun children -> (id, tags, children))))) ) st in
  ( parse_list parse_node ) st

let () =
  gc_setup() ;
  Format.set_margin (Sys.command "exit $(tput cols)") ;
  if Int.(<>) (Array.length Sys.argv) 2 then 
    begin
      print_endline usage ;
      exit 1
    end
  else
  let prf_string = Sys.argv.(1) in
  let prf = handle_reply (parse_string parse_proof prf_string ()) in
  let prf = build_proof prf in
  let res = check_proof prf in
  if res then
    begin
      print_endline "Proof is valid" ;
      exit 0 ;
    end
  else
    begin
      print_endline "Proof is NOT valid" ;
      exit 1 ;
    end
