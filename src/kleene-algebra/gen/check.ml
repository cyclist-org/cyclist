open Cmdliner
open KleeneAlg
open Lib
open MParser
open Regextkit

let rec to_re e =
  if Form.is_zero e then
    Tree.Empty
  else if Form.is_one e then
    Tree.Epsilon
  else if Form.is_letter e then
    Tree.Literal (Format.asprintf "%c" (Option.get (Form.dest_letter e)))
  else if Form.is_concat e then
    let (e', e'') = Option.get (Form.dest_concat e) in
    Tree.Concat (to_re e', to_re e'')
  else if Form.is_choice e then
    let (e', e'') = Option.get (Form.dest_choice e) in
    Tree.Union (to_re e', to_re e'')
  else if Form.is_star e then
    Tree.Star (to_re (Option.get (Form.dest_star e)))
  else
  invalid_arg __FUNCTION__

let to_dfa alphabet e =
  let dfa = Dfa.re_to_dfa (to_re e) in
  Dfa.create
    (Dfa.get_states dfa)
    (List.map Form.letter_to_string alphabet)
    (Dfa.get_transitions dfa)
    (Dfa.get_start dfa)
    (Dfa.get_accepting dfa)

let form_converter =
  let parser s =
    match (MParser.parse_string Form.parse s ()) with
    | Success res ->
      Stdlib.Ok res
    | Failed (msg, _) ->
      Stdlib.Error (`Msg msg) in
  Arg.conv ~docv:"FORMULA" (parser, Form.pp)

let check e f =
  let alphabet = Form.get_alphabet (Form.concat e f) in
  let dfa_e = to_dfa alphabet e in
  let dfa_f = to_dfa alphabet f in
  let res =
    Dfa.is_empty (Dfa.product_intersection dfa_e (Dfa.complement dfa_f)) in
  if res then
    print_endline("YES")
  else
    print_endline("NO")

let left_expr =
  let info =
    let doc = "The contained expression" in
    Arg.info ~doc [] in
  Arg.required (Arg.pos 0 (Arg.some form_converter) None info)

let right_expr =
  let info =
    let doc = "The containing expression" in
    Arg.info ~doc [] in
  Arg.required (Arg.pos 1 (Arg.some form_converter) None info)

let () =
  let cmd =
    let term = Term.(const check $ left_expr $ right_expr) in
    let doc = "Check the inclusion between two regular expressions." in
    Cmd.(v (info ~doc "check") term) in
  Cmd.(exit (eval cmd))