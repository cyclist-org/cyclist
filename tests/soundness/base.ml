open Lib
open Generic

let speclist =
  Soundcheck.arg_opts
    @
  [ ("-d", Arg.Set do_debug, ": print debug messages")
  ; ("-s", Arg.Set Stats.do_statistics, ": print statistics") ]

let usage =
  "usage: " ^ Sys.argv.(0) ^ " [-d] [-s] [-spot|-ext [-ff][-scc][-idem][-min]] <param_1> ... <param_n>"

let params = ref []

let add_param n =
  try
    let n = int_of_string n in
    if n < 1 then
      raise (Arg.Bad "Must specify a positive size parameter")
    else
      params := n :: !params
  with Failure _ ->
    raise (Arg.Bad "Must specify an integer size parameter")

let () =
  Arg.parse speclist add_param usage ;
  params := List.rev !params

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