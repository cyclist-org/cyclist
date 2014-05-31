(* Handles command-line parameters, etc. for the predicate consistency     *)
(* checker. Similar to cyclist/generic/frontend.ml, which is for           *)
(* instantiations of the generic cyclic theorem prover.                    *)

open Lib
open Symheap

let show_proof = ref false
let latex_path = ref ""
let timeout = ref 30
let only_first = ref false

let speclist = ref [
    ("-p", Arg.Set show_proof,": show proof");
    ("-d", Arg.Set do_debug,": print debug messages");
    ("-s", Arg.Set Stats.do_statistics,": print statistics");
    ("-t", Arg.Set_int timeout,
      (": set timeout in seconds to <int>, 0 disables it, default is " ^
        (string_of_int !timeout)));
    ("-f", Arg.Set only_first,": check satisfiability of first predicate only");
    ]

let usage = ref ("usage: " ^ Sys.argv.(0) ^ " [-p/d/s/f] [-t <int>]")

let die msg =
  print_endline msg ;
  print_endline (Arg.usage_string !speclist !usage) ;
  exit 1

let check_consistency defs =
  let exit_code = ref 0 in
  Format.set_margin (Sys.command "exit $(tput cols)") ;
  Stats.reset () ;
  Stats.Gen.call () ;
  let consistency_check () = Defs.consistent defs !only_first !show_proof in
  let res = w_timeout consistency_check !timeout in
  Stats.Gen.end_call () ;
  begin
    match res with
    | None ->
      begin
        print_endline "UNKNOWN: [TIMEOUT]" ;
        exit_code := 2
      end
    | Some false ->
      begin
        print_endline 
          ("UNSAT: " ^
          (if !only_first then "First" else "Some") ^ 
          " predicate has an empty base.") ;
        exit_code := 1
      end
    | Some true -> 
      print_endline 
        ("SAT: " ^ 
        (if !only_first then "First predicate has" else "All predicates have") ^
        " a non-empty base.")
  end ;
  if !Stats.do_statistics then Stats.gen_print ();
  !exit_code
