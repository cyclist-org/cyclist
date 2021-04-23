open Lib
open Generic
open Seplog

let defs_path = ref "examples/sl.defs"

let show_proof = ref false

let latex_path = ref ""

let timeout = ref 30

let only_first = ref false

let slcomp = ref ""

let speclist =
  ref
    [ ("-p", Arg.Set show_proof, ": show proof")
    ; ("-d", Arg.Set do_debug, ": print debug messages")
    ; ("-s", Arg.Set Stats.do_statistics, ": print statistics")
    ; ( "-t"
      , Arg.Set_int timeout
      , ": set timeout in seconds to <int>, 0 disables it, default is "
        ^ string_of_int !timeout )
    ; ( "-f"
      , Arg.Set only_first
      , ": check satisfiability of first predicate only" )
    ; ( "-D"
      , Arg.Set_string defs_path
      , ": read inductive definitions from <file>, default is " ^ !defs_path )
    ; ( "-SLCOMP"
      , Arg.Set_string slcomp
      , ": change input to SMTLIB <file> and output to sat/unsat/unknown for \
         SLCOMP " ^ !slcomp ) ]

let usage =
  ref
    ( "usage: " ^ Sys.argv.(0)
    ^ " [-p/d/s/f] [-t <int>] [-D <file>] [-SLCOMP <file>]" )

let die msg =
  print_endline msg ;
  print_endline (Arg.usage_string !speclist !usage) ;
  exit 1

let () =
  gc_setup () ;
  Arg.parse !speclist (fun _ -> raise (Arg.Bad "Stray argument found.")) !usage ;
  let slcomp_mode = not (String.equal "" !slcomp) in
  Format.set_margin (Sys.command "exit $(tput cols)") ;
  let consistency_check () =
    if slcomp_mode then
      let defs, f = Smtlib.defs_of_channel (open_in !slcomp) in
      Basepair.form_sat defs f
    else
      let defs = Defs.of_channel (open_in !defs_path) in
      Basepair.satisfiable ~only_first:!only_first ~output:!show_proof defs
  in
  Stats.reset () ;
  Stats.Gen.call () ;
  let res = w_timeout consistency_check !timeout in
  Stats.Gen.end_call () ;
  let slcomp_mode = not (String.equal "" !slcomp) in
  let exit_code =
    match res with
    | None ->
        print_endline (if slcomp_mode then "unknown" else "UNKNOWN: [TIMEOUT]") ;
        2
    | Some false ->
        print_endline
          ( if slcomp_mode then "unsat"
          else
            "UNSAT: "
            ^ (if !only_first then "First" else "Some")
            ^ " *inductive rule* has an empty base." ) ;
        1
    | Some true ->
        print_endline
          ( if slcomp_mode then "sat"
          else
            "SAT: "
            ^ ( if !only_first then "First predicate has"
              else "All predicates have" )
            ^ " a non-empty base." ) ;
        0
  in
  if !Stats.do_statistics then Stats.gen_print () ;
  exit (if slcomp_mode then 0 else exit_code)
