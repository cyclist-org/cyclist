open Lib

let defs_path = ref "examples/sl.defs"
let show_proof = ref false
let latex_path = ref ""
let timeout = ref 30
let only_first = ref false
let slcomp_mode = ref false

let speclist = ref [
    ("-p", Arg.Set show_proof,": show proof");
    ("-d", Arg.Set do_debug,": print debug messages");
    ("-s", Arg.Set Stats.do_statistics,": print statistics");
    ("-t", Arg.Set_int timeout,
      (": set timeout in seconds to <int>, 0 disables it, default is " ^
        (string_of_int !timeout)));
    ("-f", Arg.Set only_first,": check satisfiability of first predicate only");
    ("-D", Arg.Set_string defs_path,
       ": read inductive definitions from <file>, default is " ^ !defs_path);
    ("-SLCOMP", Arg.Set slcomp_mode, ": change output to sat/unsat/unknown for SLCOMP");
    ]

let usage = ref ("usage: " ^ Sys.argv.(0) ^ " [-p/d/s/f] [-t <int>] [-D <file>]" )

let die msg =
  print_endline msg ;
  print_endline (Arg.usage_string !speclist !usage) ;
  exit 1

let check_consistency defs =
  Format.set_margin (Sys.command "exit $(tput cols)") ;
  Stats.reset () ;
  Stats.Gen.call () ;
  let consistency_check () = 
    Sl_basepair.satisfiable ~only_first:!only_first ~output:!show_proof defs in
  let res = w_timeout consistency_check !timeout in
  Stats.Gen.end_call () ;
  let exit_code = 
    match res with
    | None ->
      begin
        print_endline 
          (if !slcomp_mode then
            "unknown"
          else
            "UNKNOWN: [TIMEOUT]") ;
        2
      end
    | Some false ->
      begin
        print_endline 
          (if !slcomp_mode then 
            "unsat"
          else
            ("UNSAT: " ^
            (if !only_first then "First" else "Some") ^ 
            " *inductive rule* has an empty base.")) ;
        1
      end
    | Some true -> 
      print_endline 
        (if !slcomp_mode then
          "sat"
        else
          ("SAT: " ^ 
          (if !only_first then "First predicate has" else "All predicates have") ^
          " a non-empty base.")) ;
      0
  in
  if !Stats.do_statistics then Stats.gen_print ();
  if !slcomp_mode then 0 else exit_code
    


let () =
  gc_setup () ;
	Arg.parse !speclist (fun _ -> raise (Arg.Bad "Stray argument found.")) !usage ;
	let res = check_consistency (Sl_defs.of_channel (open_in !defs_path)) in
  exit res


