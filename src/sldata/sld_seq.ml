open Lib
open Util
open Symbols
open MParser

include PairTypes(Sld_form)(Sld_form)

let equal (l,r) (l',r') =
  Sld_form.equal l l' 
  &&
  Sld_form.equal_upto_tags r r'

let equal_upto_tags (l,r) (l',r') =
  Sld_form.equal_upto_tags l l' 
  &&
  Sld_form.equal_upto_tags r r'


let dest seq = Pair.map Sld_form.dest seq

let parse st =
  ( Sld_form.parse >>= (fun l ->
          parse_symb symb_turnstile >> Sld_form.parse >>= (fun r ->
                return (l, r))) <?> "Sequent") st

let of_string s =
  handle_reply (MParser.parse_string parse s ())

let to_string (l, r) =
  (Sld_form.to_string l) ^ symb_turnstile.sep ^ (Sld_form.to_string r)
let to_melt (l, r) =
  ltx_mk_math
    (Latex.concat [Sld_form.to_melt l; symb_turnstile.melt; Sld_form.to_melt r])

let pp fmt (l, r) =
  Format.fprintf fmt "@[%a %s@ %a@]" Sld_form.pp l symb_turnstile.str Sld_form.pp r

let terms (l, r) = Sld_term.Set.union (Sld_form.terms l) (Sld_form.terms r)
let vars seq = Sld_term.filter_vars (terms seq)

let tags seq = Sld_form.tags (fst seq)
let tag_pairs f = TagPairs.mk (tags f)

let subst theta seq = Pair.map (Sld_form.subst theta) seq

let subst_tags tagpairs (l,r) = (Sld_form.subst_tags tagpairs l, r)

(* (l',r') *)
(* ------- *)
(* (l,r)   *)
(* meaning l  |- l' *)
(* and     r' |- r  *)

let subsumed (l,r) (l',r') = 
  Sld_form.subsumed l' l && Sld_form.subsumed_upto_tags r r'

let subsumed_upto_tags (l,r) (l',r') = 
  Sld_form.subsumed_upto_tags l' l && Sld_form.subsumed_upto_tags r r'

let norm s = Pair.map Sld_form.norm s               
