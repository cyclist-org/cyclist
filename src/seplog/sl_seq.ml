open Lib
open Util
open Symbols
open MParser

include PairTypes(Sl_form)(Sl_form)

let dest seq = Pair.map Sl_form.dest seq

let to_string (l, r) =
  (Sl_form.to_string l) ^ symb_turnstile.sep ^ (Sl_form.to_string r)
let to_melt (l, r) =
  ltx_mk_math
    (Latex.concat [Sl_form.to_melt l; symb_turnstile.melt; Sl_form.to_melt r])

let pp fmt (l, r) =
  Format.fprintf fmt "@[%a %s@ %a@]" Sl_form.pp l symb_turnstile.str Sl_form.pp r

let vars (l, r) =
  Sl_term.filter_vars (Sl_term.Set.union (Sl_form.terms l) (Sl_form.terms r))
let tags (seq: t) = Sl_form.tags (fst seq)
let tag_pairs f = TagPairs.mk (tags f)

let subst theta seq = Pair.map (fun f -> Sl_form.subst theta f) seq

(* s2 entails s1 *)
(* let subsumed_wrt_tags t s1 s2 =                          *)
(*   Sl_form.subsumed_wrt_tags t (fst s2) (fst s1) &&       *)
(*   Sl_form.subsumed_wrt_tags Tags.empty (snd s1) (snd s2) *)
(* s' ___ s *)
let uni_subsumption s s' =
  failwith "FIXME"
  (* let ((l, r), (l', r')) = (s, s') in                                  *)
  (* let tags = Tags.inter (tags s) (tags s') in                          *)
  (* let valid theta' =                                                   *)
  (*   let s'' = subst theta' s' in                                       *)
  (*   let tags' = Tags.fold                                              *)
  (*       ( fun t acc ->                                                 *)
  (*             let new_acc = Tags.add t acc in                          *)
  (*             if subsumed_wrt_tags new_acc s s'' then new_acc else acc *)
  (*       ) tags Tags.empty in                                           *)
  (*   if not (Tags.is_empty tags') then                                  *)
  (*     Some theta' else None in                                         *)
  (* let hook theta' = Sl_form.right_subsumption valid theta' r r' in     *)
  (* Sl_form.left_subsumption hook Sl_term.Map.empty l' l                 *)

let parse st =
  ( Sl_form.parse >>= (fun l ->
          parse_symb symb_turnstile >> Sl_form.parse >>= (fun r ->
                return (l, r))) <?> "Sequent") st

let of_string s =
  handle_reply (MParser.parse_string parse s ())
