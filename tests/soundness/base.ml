open Lib
open Generic

let speclist =
  [ ("-spot", Arg.Set Soundcheck.use_spot, ": use the spot model checker to verify the cyclic trace condition")
  ; ("-ext", Arg.Set Soundcheck.use_external, ": use external C++ code to verify the cyclic trace condition")
  ; ("-ff", Arg.Unit Soundcheck.fail_fast, ": use fast fail in external C++ code")
  ; ("-scc", Arg.Unit Soundcheck.use_scc_check, ": use SCC check in external C++ code")
  ; ("-idem", Arg.Unit Soundcheck.use_idempotence, ": use idempotency optimisation in external C++ code")
  ; ("-min", Arg.Unit Soundcheck.use_minimality, ": use minimality optimisation in external C++ code")
  ; ("-d", Arg.Set do_debug, ": print debug messages")
  ; ("-s", Arg.Set Stats.do_statistics, ": print statistics") ]

let usage =
  "usage: " ^ Sys.argv.(0) ^ " [-d] [-s] [-spot|-ext [-ff][-scc][-idem][-min]] [<size>]"

let size = ref 1

let set_num_nodes n =
  try
    let n = int_of_string n in
    if n < 1 then
      raise (Arg.Bad "Must specify a positive size parameter")
    else
      size := n
  with Failure _ ->
    raise (Arg.Bad "Must specify an integer size parameter")

let () =
  Arg.parse speclist set_num_nodes usage

let runtest ?(minimize=true) build_prf =
  let () = gc_setup () in
  let () = Format.set_margin (Sys.command "exit $(tput cols)") in
  let () = Stats.reset () in
  let () = Stats.Gen.call () in
  let prf = build_prf () in
  let res = Soundcheck.check_proof ~minimize prf in
  let () = Stats.Gen.end_call () in
  let () = if !Stats.do_statistics then Stats.gen_print () in
  match res with
  | true ->
    print_endline "OK"
  | false ->
    print_endline "NOT OK";
    Stdlib.exit(1)