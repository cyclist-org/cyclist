open Lib
open   Symbols

open Generic

open MParser

include Pair.Make (Sl_form) (Sl_form)

let equal (l, r) (l', r') = Sl_form.equal l l' && Sl_form.equal r r'

let equal_upto_tags (l, r) (l', r') =
  Sl_form.equal_upto_tags l l' && Sl_form.equal_upto_tags r r'

let dest seq = Pair.map Sl_form.dest seq

let to_string (l, r) =
  Sl_form.to_string l ^ symb_turnstile.sep ^ Sl_form.to_string r

let pp fmt (l, r) =
  Format.fprintf fmt "@[%a %s@ %a@]" Sl_form.pp l symb_turnstile.str Sl_form.pp
    r

let parse ?(null_is_emp = false) st =
  ( Sl_form.parse ~null_is_emp
  >>= (fun l ->
        parse_symb symb_turnstile
        >> ( Sl_form.parse ~null_is_emp
           >>= fun r ->
           let tags = Tags.union (Sl_form.tags l) (Sl_form.tags r) in
           let l' = Sl_form.complete_tags tags l in
           let inst_subst =
             Tagpairs.mk_free_subst tags (Tags.diff (Sl_form.tags l') tags)
           in
           let l' = Sl_form.subst_tags inst_subst l' in
           let r' = Sl_form.complete_tags tags r in
           return (l', r') ) )
  <?> "Sequent" )
    st

let of_string ?(null_is_emp = false) s =
  handle_reply (MParser.parse_string (parse ~null_is_emp) s ())

let terms (l, r) = Sl_term.Set.union (Sl_form.terms l) (Sl_form.terms r)

let vars seq = Sl_term.filter_vars (terms seq)

let tags (l, r) = Tags.union (Sl_form.tags l) (Sl_form.tags r)

let tag_pairs (l, _) = Tagpairs.mk (Sl_form.tags l)

let get_tracepairs (l, _) (l', _) =
  let tps = Sl_form.get_tracepairs l l' in
  Pair.map (Tagpairs.filter (fun (t, _) -> Tags.is_free_var t)) tps

let subst theta seq = Pair.map (Sl_form.subst theta) seq

let subst_tags tagpairs (l, r) =
  (Sl_form.subst_tags tagpairs l, Sl_form.subst_tags tagpairs r)

(* (l',r') *)
(* ------- *)
(* (l,r)   *)
(* meaning l  |- l' *)
(* and     r' |- r  *)

let subsumed (l, r) (l', r') = Sl_form.subsumed l' l && Sl_form.subsumed r r'

let subsumed_upto_tags (l, r) (l', r') =
  Sl_form.subsumed_upto_tags l' l && Sl_form.subsumed_upto_tags r r'

let norm s = Pair.map Sl_form.norm s
