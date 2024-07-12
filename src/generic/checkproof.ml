open MParser
open   MParser_RE

open Lib
open   Parsers

open Generic
open   Soundcheck

let batch_mode = ref false
let allow_comments = ref false

let input_files = ref []

let minimize = ref true

let speclist =
  Soundcheck.arg_opts
    @
  [
    ("-d", Arg.Set do_debug, ": print debug messages") ;
    ("-s", Arg.Set Stats.do_statistics, ": print statistics") ;
    ("--unminimized-proofs", Arg.Clear minimize, ": keep proofs unminimized") ;
    ("--allow-comments", Arg.Set allow_comments, ": allow line comments in input") ;
    ("-f", Arg.String (fun f -> input_files := f :: !input_files), ": take input from file") ;
  ]

let usage =
  "usage: " ^ Sys.argv.(0) ^ " [-d] [-s] [-R (node|edge|json)] [ --allow-comments | (-f <file>)* ] [ -VLA | -SLA | -legacy | -OR | -FWK [-ff][-scc][-idem][-min][-rel-stats] ]"

let () =
  Arg.parse speclist (fun _ -> ()) usage

let do_check prf =
  begin
    Stats.reset ();
    if (check_proof ~minimize:!minimize prf) then begin
      print_endline "YES";
      Stats.gen_print ();
    end
    else begin
      print_endline "NO";
      Stats.gen_print ();
    end
  end

let process_files parser =
  let process_file f =
    let f_in = open_in f in
    let prfs =
      handle_reply (parse_channel parser f_in ()) in
    let () = close_in f_in in
    List.iter do_check prfs in
  List.iter process_file (List.rev !input_files)

let process_stdin parser =
  let buf = Buffer.create 2014 in
  let next_char =
    match !allow_comments with
    | true ->
      let comment = ref false in
      (fun () ->
        let c = input_char stdin in
        let () =
          if not !comment && Char.equal c '#'
            then comment := true
          else if Char.equal c '\n'
            then comment := false in
        let () =
          if not !comment then Buffer.add_char buf c in
        not !comment && Char.equal c ';')
    | false ->
      (fun () ->
        let c = input_char stdin in
        let () = Buffer.add_char buf c in
        Char.equal c ';')
    in
  while true do
    let ready =
      try
        next_char ()
      with End_of_file -> true in
    if ready then
      let input = Buffer.contents buf in
      let () = Buffer.clear buf in
      match (parse_string (spaces >> eof) input ()) with
      | Success _ ->
        (* if input consists of nothing but spaces *)
        exit 0
      | Failed _ ->
        let prfs =
          handle_reply (parse_string parser input ()) in
        List.iter do_check prfs
    done

let () =
  let () = gc_setup () in
  let () = Format.set_margin (Sys.command "exit $(tput cols)") in
  let module Repr : Soundcheck.Representation = (val Soundcheck.representation ()) in
  let parser =
    sep_end_by
      (spaces >> Repr.parse << spaces)
      ((skip Tokens.semi << spaces) <|> spaces) in
  match !input_files with
  | [] ->
    process_stdin parser
  | _ ->
    process_files parser
