open Lib
open   Symbols

open Generic

open MParser

type t = Form.t * Form.t

let equal (l,r) (l',r') = Form.equal l l' && Form.equal r r'
let equal_upto_tags (l,r) (l',r') =
  Form.equal_upto_tags l l' && Form.equal_upto_tags r r'
let dest s = Pair.map Form.dest s
let tags seq = Form.tags (fst seq)
let to_string (l,r) = (Form.to_string l) ^ " |- " ^ (Form.to_string r)
let pp fmt (l,r) = Format.fprintf fmt "@[%a |-@ %a@]" Form.pp l Form.pp r
let parse st =
  ( Form.parse >>= (fun l ->
    parse_symb symb_turnstile >>
    Form.parse >>= (fun r ->
    return (l,r))) <?> "Seq") st

let of_string s =
  handle_reply (MParser.parse_string parse s ())

let terms s = Term.Set.union (Form.terms (fst s)) (Form.terms (snd s))
let vars s = Term.filter_vars (terms s)
let tag_pairs f = Tagpairs.mk (tags f)
(* s2 entails s1 *)
let subsumed_wrt_tags t s1 s2 =
  Form.subsumed_wrt_tags t (fst s2) (fst s1) &&
  Form.subsumed_wrt_tags Tags.empty (snd s1) (snd s2)

let subst theta s = Pair.map (Form.subst theta) s

(*  s' *)
(* ___ *)
(*  s  *)
let uni_subsumption s s' =
  let ((l,r),(l',r')) = (s,s') in
  let tags = Tags.inter (tags s) (tags s') in
  let valid theta' =
    let s'' = subst theta' s' in
    let tags' = Tags.fold
      ( fun t acc ->
        let new_acc = Tags.add t acc in
        if subsumed_wrt_tags new_acc s s'' then new_acc else acc
      ) tags Tags.empty in
    if not (Tags.is_empty tags') then
      Some theta' else None in
  let hook theta' = Form.right_subsumption valid theta' r r' in
  Form.left_subsumption hook Term.Map.empty l' l
