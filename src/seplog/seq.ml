open Lib
open   Symbols

open Generic

open MParser

include Pair.Make (Form) (Form)

let equal (l, r) (l', r') = Form.equal l l' && Form.equal r r'

let equal_upto_tags (l, r) (l', r') =
  Form.equal_upto_tags l l' && Form.equal_upto_tags r r'

let dest seq = Pair.map Form.dest seq

let to_string (l, r) =
  Form.to_string l ^ symb_turnstile.sep ^ Form.to_string r

let pp fmt (l, r) =
  Format.fprintf fmt "@[%a %s@ %a@]" Form.pp l symb_turnstile.str Form.pp
    r

let parse ?(null_is_emp = false) st =
  ( Form.parse ~null_is_emp
  >>= (fun l ->
        parse_symb symb_turnstile
        >> ( Form.parse ~null_is_emp
           >>= fun r ->
           let tags = Tags.union (Form.tags l) (Form.tags r) in
           let l' = Form.complete_tags tags l in
           let inst_subst =
             Tagpairs.mk_free_subst tags (Tags.diff (Form.tags l') tags)
           in
           let l' = Form.subst_tags inst_subst l' in
           let r' = Form.complete_tags tags r in
           return (l', r') ) )
  <?> "Sequent" )
    st

let of_string ?(null_is_emp = false) s =
  handle_reply (MParser.parse_string (parse ~null_is_emp) s ())

let terms (l, r) = Term.Set.union (Form.terms l) (Form.terms r)

let vars seq = Term.filter_vars (terms seq)

let tags (l, r) = Tags.union (Form.tags l) (Form.tags r)

let tag_pairs (l, _) = Tagpairs.mk (Form.tags l)

let get_tracepairs (l, _) (l', _) =
  let tps = Form.get_tracepairs l l' in
  Pair.map (Tagpairs.filter (fun (t, _) -> Tags.is_free_var t)) tps

let subst theta seq = Pair.map (Form.subst theta) seq

let subst_tags tagpairs (l, r) =
  (Form.subst_tags tagpairs l, Form.subst_tags tagpairs r)

(* (l',r') *)
(* ------- *)
(* (l,r)   *)
(* meaning l  |- l' *)
(* and     r' |- r  *)

let subsumed (l, r) (l', r') = Form.subsumed l' l && Form.subsumed r r'

let subsumed_upto_tags (l, r) (l', r') =
  Form.subsumed_upto_tags l' l && Form.subsumed_upto_tags r r'

let norm s = Pair.map Form.norm s
