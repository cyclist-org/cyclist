open MParser
open   MParser_RE

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

  The first tag pair list are the progressing tags, and the second list are the
  non-progressing tags.

  Lines beginning with the same node id are merged.

  Proofs must end with a semicolon.

  A value of Soundcheck.t is returned.
*)

let id_exp =
  make_regexp "[_0-9a-zA-Z'\\.\\-]+"

let parse_nodeid s =
  (regexp id_exp << spaces <?> "Identifier" |>> Node.mk) s

let parse_tagid s =
  (regexp id_exp << spaces |>> Tags.mk) s

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
  (many parse_line << Tokens.semi |>> mk_prf) s

let parse_element_id s =
  (((string "Node") >>$ `Nodes)
    <|>
   ((string "Edge") >>$ `Edges)
    <|>
   ((string "Height") >>$ `Heights)) s

let parse_element s =
  let parse_tag_list s =
    (Tokens.comma_sep (spaces >> (regexp id_exp) << spaces)) s in
  let parse_int_pair s =
    (Tokens.squares (
      spaces >>
      Tokens.decimal >>= (fun x ->
      spaces >>
      Tokens.comma >>
      spaces >>
      Tokens.decimal << spaces |>> (fun y ->
      (x, y))))) s in
  let parse_slope_triple s =
    (Tokens.squares (
      spaces >>
      (regexp id_exp) << spaces >>= (fun x ->
      Tokens.comma >>
      (regexp id_exp) << spaces >>= (fun y ->
      Tokens.comma >>
      Tokens.decimal << spaces |>> (fun z ->
      (x, y, z)))))) s in
  let parse_node s =
    (Tokens.squares (
      spaces >>
      Tokens.decimal >>= (fun node_id ->
      spaces >>
      Tokens.comma >>
      spaces >>
      (Tokens.squares parse_tag_list) << spaces
      |>> (fun heights -> (node_id, heights))))) s in
  let parse_edge s =
    (Tokens.squares (
      spaces >>
      parse_int_pair >>= (fun edge ->
      spaces >>
      Tokens.comma >>
      spaces >>
      Tokens.squares (spaces >> (Tokens.comma_sep (parse_slope_triple)) << spaces)
      |>> (fun slopes -> (edge, slopes))))) s in
  ((between (char '"') (char '"') parse_element_id <?> "Node | Edge | Height") >>= (fun id ->
   spaces >>
   Tokens.colon >>
   spaces >>
   Tokens.squares
    (match id with
     | `Heights ->
        parse_tag_list
        |>> (fun heights -> `Heights heights)
     | `Nodes ->
        Tokens.comma_sep (spaces >> parse_node << spaces)
        |>> (fun nodes -> `Nodes nodes)
     | `Edges ->
       Tokens.comma_sep (spaces >> parse_edge << spaces)
       |>> (fun edges -> `Edges edges))
  )) s

let parse_json_proof s =
  let mk_prf nodes edges =
    let abs_nodes =
      List.fold_left
        (fun abs_nodes (node, heights) ->
          if Int.Map.mem node abs_nodes then
            let () =
              prerr_endline
                (Format.sprintf
                  "Warning: ignoring duplicate node declaration: %i" node) in
            abs_nodes
          else
            Int.Map.add node ((List.map Tags.mk heights), [], []) abs_nodes)
        (Int.Map.empty)
        (nodes) in
    let abs_nodes =
      List.fold_left
        (fun abs_nodes ((src, dest), slopes) ->
          Int.Map.update
            (src)
            (Option.map
              (fun ((heights, succs, slopes') as binding) ->
                (* Duplicates in this list are not handled by the
                   proof minimisation procedure *)
                if List.mem dest succs then
                  let () =
                    prerr_endline
                      (Format.sprintf
                        "Warning: ignoring duplicate edge declaration (%i, %i)"
                        src dest) in
                  binding
                else
                  let tps, prog_tps =
                    List.fold_left
                      (fun (tps, prog_tps) (src_height, dst_height, slope) ->
                        let tp = (Tags.mk src_height, Tags.mk dst_height) in
                        let tps = tp :: tps in
                        if Int.equal slope 1
                          then (tps, tp::prog_tps)
                          else (tps, prog_tps))
                      ([], [])
                      (slopes) in
                  let tps = Tagpairs.of_list tps in
                  let prog_tps = Tagpairs.of_list prog_tps in
                  (heights, dest::succs, (tps, prog_tps)::slopes')))
            (abs_nodes))
        (abs_nodes)
        (edges) in
    let prf =
      Int.Map.map
        (fun (heights, succs, slopes) ->
            mk_abs_node (Tags.of_list heights) succs slopes)
        (abs_nodes) in
    let init =
      match nodes with
      | [] ->
        0
      | (node_id, _)::_ ->
        node_id in
    (prf, init) in
  let mk_prf =
    function
    | (heights, Some nodes, Some edges) ->
      let () =
        Option.iter
          (List.iter (fun height -> let _ = Tags.mk height in ()))
          (heights) in
      mk_prf nodes edges
    | _ ->
      invalid_arg "Missing information!" in
  let fold_elem (heights, nodes, edges) =
    function
    | `Heights heights ->
      (Some heights, nodes, edges)
    | `Nodes nodes ->
      (heights, Some nodes, edges)
    | `Edges edges ->
      (heights, nodes, Some edges) in
  (Tokens.braces (Tokens.comma_sep (spaces >> parse_element << spaces))
    |>> (fun elems ->
          mk_prf (List.fold_left fold_elem (None, None, None) elems))) s

let batch_mode = ref false
let allow_comments = ref false

type input_mode_t =
| EDGE_LIST
| JSON

let input_mode = ref EDGE_LIST

let speclist =
  Soundcheck.arg_opts
    @
  [
    ("-d", Arg.Set do_debug, ": print debug messages") ;
    ("--json", Arg.Unit (fun () -> input_mode := JSON), "Use JSON input format") ;
    ("--batch", Arg.Set batch_mode, ": batch processing mode") ;
    ("--allow-comments", Arg.Set allow_comments, ": allow line comments in input") ;
  ]

let usage =
  "usage: " ^ Sys.argv.(0) ^ " [-d] [-s] [-spot|-rel-ext|SD [-full][-ff][-scc][-idem][-min][-rel-stats]]"

let () =
  Arg.parse speclist (fun _ -> ()) usage

let do_check (prf, init) =
  if (check_proof ~init prf) then
    print_endline "YES"
  else
    print_endline "NO"

let () =
  let () = gc_setup () in
  let () = Format.set_margin (Sys.command "exit $(tput cols)") in
  match !batch_mode, !allow_comments with
  | true, false ->
    let parser =
      match !input_mode with
      | EDGE_LIST ->
        many (spaces >> parse_proof)
      | JSON ->
        many (spaces >> parse_json_proof)
      in
    let prfs =
      handle_reply (parse_channel parser stdin ()) in
    List.iter do_check prfs
  | _ ->
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
          let parser =
            match !input_mode with
            | EDGE_LIST ->
              spaces >> parse_proof
            | JSON ->
              spaces >> parse_json_proof << spaces << Tokens.semi in
          do_check (handle_reply (parse_string parser input ()))
    done
