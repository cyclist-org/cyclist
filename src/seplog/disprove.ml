open Lib
open Generic
open Seplog

let cl_sequent = ref ""

let defs_path = ref "examples/sl.defs"

let z3 = ref false

let show_proof = ref false

let usage =
  "usage: " ^ Sys.argv.(0)
  ^ " [-p/d/s/f/-Z] [-t <int>] [-D <file>] [-S <string>]"

let timeout = ref 60

let speclist =
  [ ("-p", Arg.Set show_proof, ": show proof")
  ; ("-d", Arg.Set do_debug, ": print debug messages")
  ; ("-s", Arg.Set Stats.do_statistics, ": print statistics")
  ; ( "-t"
    , Arg.Set_int timeout
    , ": set timeout in seconds to <int>, 0 disables it, default is "
      ^ string_of_int !timeout )
  ; ( "-D"
    , Arg.Set_string defs_path
    , ": read inductive definitions from <file>, default is " ^ !defs_path )
  ; ( "-S"
    , Arg.Set_string cl_sequent
    , ": disprove the SL sequent provided in <string>" )
  ; ( "-IP"
    , Arg.Set Invalid.partition_strengthening
    , ": use partition strengthening in invalidity heuristic, default is "
      ^ string_of_bool !Invalid.partition_strengthening )
  ; ("-Z", Arg.Set z3, ": only generate Z3 input") ]

let die msg =
  print_endline msg ;
  print_endline (Arg.usage_string speclist usage) ;
  exit 1

let () =
  gc_setup () ;
  Format.set_margin (Sys.command "exit $(tput cols)") ;
  Arg.parse speclist (fun _ -> raise (Arg.Bad "Stray argument found.")) usage ;
  if String.equal !cl_sequent "" then die "-S must be specified." ;
  let seq = Seq.of_string !cl_sequent in
  let defs = Defs.of_channel (open_in !defs_path) in
  Rules.setup defs ;
  (* if !z3 then                  *)
  (*   Invalid.to_z3 defs seq  *)
  (* else *)
  Stats.reset () ;
  Stats.Gen.call () ;
  let call () = Invalid.invalidity_witness defs seq in
  let res =
    if not (Int.equal !timeout 0) then w_timeout call !timeout
    else Some (call ())
  in
  Stats.Gen.end_call () ;
  if !Stats.do_statistics then Stats.gen_print () ;
  let exit_code =
    match res with
    | None ->
        print_endline ("UNKNOWN: " ^ Seq.to_string seq ^ " [TIMEOUT]") ;
        2
    | Some (Some bp) ->
        print_endline ("INVALID: " ^ Seq.to_string seq) ;
        if !show_proof then
          Format.printf "INVALID witness: %a\n" Basepair.pp bp ;
        255
    | Some None ->
        print_endline ("UNKNOWN: " ^ Seq.to_string seq) ;
        1
  in
  exit exit_code
