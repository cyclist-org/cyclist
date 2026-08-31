open MParser
open MParser_RE
open Lib
open Soundcheck

let do_check prf =
  begin
    Stats.reset ();
    if check_proof prf then begin
      print_endline "YES";
      Stats.gen_print ()
    end
    else begin
      print_endline "NO";
      Stats.gen_print ()
    end
  end

let process_files parser input_files =
  let process_file f =
    let f_in = open_in f in
    let prfs = handle_reply (parse_channel parser f_in ()) in
    let () = close_in f_in in
    List.iter do_check prfs
  in
  List.iter process_file input_files

let process_stdin parser allow_comments =
  let buf = Buffer.create 2014 in
  let next_char =
    match allow_comments with
    | true ->
        let comment = ref false in
        fun () ->
          let c = input_char stdin in
          let () =
            if (not !comment) && Char.equal c '#' then comment := true
            else if Char.equal c '\n' then comment := false
          in
          let () = if not !comment then Buffer.add_char buf c in
          (not !comment) && Char.equal c ';'
    | false ->
        fun () ->
          let c = input_char stdin in
          let () = Buffer.add_char buf c in
          Char.equal c ';'
  in
  while true do
    let ready = try next_char () with End_of_file -> true in
    if ready then
      let input = Buffer.contents buf in
      let () = Buffer.clear buf in
      match parse_string (spaces >> eof) input () with
      | Success _ ->
          (* if input consists of nothing but spaces *)
          exit 0
      | Failed _ ->
          let prfs = handle_reply (parse_string parser input ()) in
          List.iter do_check prfs
  done

let run allow_comments input_files () () =
  let () = gc_setup () in
  let parser =
    sep_end_by
      (spaces >> parse << spaces)
      (skip Tokens.semi << spaces <|> spaces)
  in
  match input_files with
  | [] -> process_stdin parser allow_comments
  | _ -> process_files parser input_files

let cmd =
  let open Cmdliner in
  let allow_comments =
    Arg.(
      value & flag
      & info [ "allow-comments" ] ~doc:"Allow line comments in the input.")
  in
  let input_files =
    Arg.(
      value & opt_all file []
      & info [ "f"; "file" ] ~docv:"FILE"
          ~doc:
            "Take input from $(docv). Repeatable; without it the proofs are \
             read from standard input.")
  in
  Cmd.v
    (Cmd.info "checkproof"
       ~doc:"Validate a serialised proof against the trace condition."
       ~exits:Cmd.Exit.defaults)
    Term.(
      const run $ allow_comments $ input_files $ Frontend.debug_term
      $ Soundcheck.term)
