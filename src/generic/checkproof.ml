open MParser
open MParser_PCRE

open Lib
open   Parsers

open Generic
open   Soundcheck

module Node =
  (val (VarManager.mk 0 "" (fun _ -> VarManager.FREE)) : VarManager.S)

(*
  We parse lines of the form

    <node-id> -> <node-id> : { <tag-pair-list> }, { <tag-pair-list> }

  where <tag-pair-list> is a comma-separated list of elements of the form

    (<tag-id>, <tag-id>)

  Lines beginning with the same node id are merged.

  A value of Soundcheck.t is returned.
*)

let id_exp =
  MParser_PCRE.make_regexp "[_0-9a-zA-Z'\\.\\-]+"

let parse_nodeid s =
  (MParser_PCRE.regexp id_exp << spaces <?> "Identifier" |>> Node.mk) s

let parse_tagid s =
  (MParser_PCRE.regexp id_exp << spaces |>> Tags.mk) s

let parse_tps s =
  ((sep_by
    (Tokens.parens
      (parse_tagid >>= (fun src_tag ->
       Tokens.comma >>
       parse_tagid |>> (fun dest_tag ->
       (src_tag, dest_tag)))))
    Tokens.comma) |>> Tagpairs.of_list
  ) s

let parse_line s =
  (
    parse_nodeid >>= (fun src ->
    (string "->") >> spaces >>
    parse_nodeid >>= (fun dest ->
    Tokens.colon >>
    Tokens.braces parse_tps >>= (fun prog ->
    Tokens.comma >>
    Tokens.braces parse_tps |>> (fun nonprog ->
    (src, dest, prog, nonprog)))))
  ) s

let mk_prf lines =
  let open Int.Map in
  let add_entry nodemap (src, dest, prog, nonprog) =
    let src = Node.Var.to_int src in
    let dest = Node.Var.to_int dest in
    let all = Tagpairs.union nonprog prog in
    let src_tags = Tagpairs.projectl all in
    let dest_tags = Tagpairs.projectr all in
    let () =
      assert (
        match (find_opt src nodemap) with
        | None ->
          true
        | Some (_, premises, _) ->
          not (List.mem dest premises)
      ) in
    let nodemap =
      update
        src
        (function
          | None ->
            Some (src_tags, [dest], [(all, prog)])
          | Some (tags, premises, tps) ->
            Some
              (Tags.union src_tags tags, dest::premises, (all, prog)::tps))
        nodemap in
    let nodemap =
      update
        dest
        (function
          | None ->
            Some (dest_tags, [], [])
          | Some (tags, premises, tps) ->
            Some (Tags.union dest_tags tags, premises, tps))
        nodemap in
    nodemap in
  let prf =
    map
      (fun (tags, premises, tps) -> Soundcheck.mk_abs_node tags premises tps)
      (List.fold_left add_entry empty lines) in
  let init = 
    match lines with
    | [] ->
      0
    | (src, _, _, _)::_ ->
      Node.Var.to_int src in
  (prf, init)

let parse_proof s =
  (many parse_line |>> mk_prf) s

let () =
  let () = gc_setup () in
  let () = Format.set_margin (Sys.command "exit $(tput cols)") in
  let buf = Buffer.create 2014 in
  while true do
    let ready = 
      try
        let c = input_char stdin in
        let () = Buffer.add_char buf c in
        Char.equal c ';'
      with End_of_file -> true in
    if ready then
      let input = Buffer.contents buf in
      let () = Buffer.clear buf in
      match (parse_string (spaces >> eof) input ()) with
      | Success _ ->
        exit 0
      | Failed _ ->
        let (prf, init) =
          handle_reply
            (parse_string (spaces >> parse_proof << Tokens.semi) input ()) in
        if (check_proof ~init prf)
          then print_endline "YES"
          else print_endline "NO"
  done
