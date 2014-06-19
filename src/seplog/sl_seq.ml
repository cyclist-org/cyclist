open Lib
open Util
open Symbols
open Symheap
open MParser

include PairTypes(Form)(Form)

let dest seq = Pair.map Form.dest seq

let to_string (l, r) =
  (Form.to_string l) ^ symb_turnstile.sep ^ (Form.to_string r)
let to_melt (l, r) =
  ltx_mk_math
    (Latex.concat [Form.to_melt l; symb_turnstile.melt; Form.to_melt r])

let pp fmt (l, r) =
  Format.fprintf fmt "@[%a %s@ %a@]" Form.pp l symb_turnstile.str Form.pp r

let vars (l, r) =
  Term.filter_vars (Term.Set.union (Form.terms l) (Form.terms r))
let tags (seq: t) = Form.tags (fst seq)
let tag_pairs f = TagPairs.mk (tags f)

let subst theta seq = Pair.map (fun f -> Form.subst theta f) seq

(* s2 entails s1 *)
let subsumed_wrt_tags t s1 s2 =
  Form.subsumed_wrt_tags t (fst s2) (fst s1) &&
  Form.subsumed_wrt_tags Tags.empty (snd s1) (snd s2)
(* s' ___ s *)
let uni_subsumption s s' =
  let ((l, r), (l', r')) = (s, s') in
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

let norm s = Pair.map Form.norm s

let parse st =
  ( Form.parse >>= (fun l ->
          parse_symb symb_turnstile >> Form.parse >>= (fun r ->
                return (l, r))) <?> "Sequent") st

let of_string s =
  handle_reply (MParser.parse_string parse s ())
