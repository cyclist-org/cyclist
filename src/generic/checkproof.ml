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

let parse_proof s =
  let mk_prf lines =
    let open Int.Map in
    let nodemap =
      List.fold_left
        (fun nodemap (src, dest, prog, nonprog) -> 
          let src = Node.Var.to_int src in
          let dest = Node.Var.to_int dest in
          let all = Tagpairs.union nonprog prog in
          let tags = Tagpairs.projectl all in
          update
            src
            (function
              | None ->
                Some (tags, [dest], [(all, prog)])
              | Some (tags', premises, tps) ->
                Some (Tags.union tags tags', dest::premises, (all, prog)::tps))
            nodemap)
        empty
        lines in
    let missing =
      List.fold_left
        (fun missing (_, (_, premises, tps)) ->
          List.fold_left2
            (fun missing premise (all, _) ->
              if (not (mem premise nodemap)) then
                let tags = Tagpairs.projectr all in
                update
                  premise
                  (function
                    | None ->
                      Some (tags, [], [])
                    | Some (tags', premises, tps) ->
                      Some (Tags.union tags tags', premises, tps))
                  missing
              else
                missing)
            missing
            premises
            tps)
        empty
        (bindings nodemap) in
    let nodemap = union nodemap missing in
    let prf =
      Int.Map.map
        (fun (tags, premises, tps) -> Soundcheck.mk_abs_node tags premises tps)
        nodemap in
    let init = 
      match lines with
      | [] ->
        0
      | (src, _, _, _)::_ ->
        Node.Var.to_int src in
    (prf, init) in
  (many parse_line |>> mk_prf) s

;;

gc_setup () ;;

Format.set_margin (Sys.command "exit $(tput cols)") ;;

handle_reply
    (parse_channel
      (many
        (parse_proof << Tokens.semi |>> (fun (prf, init) ->
          if (check_proof ~init prf)
            then print_endline "YES"
            else print_endline "NO"
        ))
        >> spaces >> eof)
      (stdin)
      ())

;;
