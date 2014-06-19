open Lib
open Util
open Symbols
open Symheap
open MParser

include PairTypes(Sl_heap)(IndSubf)
let mk f i : t = (f, i)
let dest (c : t) = c
let vars ((f, (_, vs)): t) =
  Term.filter_vars (Term.Set.union (Term.Set.of_list vs) (Sl_heap.terms f))
let subst theta ((f, (ident, vs)): t) =
  (Sl_heap.subst theta f, (ident, Blist.map (Term.subst theta) vs))
let freshen varset case =
  let casevars = vars case in
  let theta = Term.avoid_theta varset casevars in
  subst theta case

let to_string (f, (ident, vs)) =
  (Sl_heap.to_string f) ^ symb_ind_implies.sep ^
  ident ^ symb_lp.str ^ (Blist.to_string symb_comma.str Term.to_string vs) ^
  symb_rp.str

let pp fmt (f, (ident, vs)) =
  Format.fprintf fmt "@[%a%s%s%s%s%s@]"
    Sl_heap.pp f
    symb_ind_implies.sep
    ident
    symb_lp.str
    (Blist.to_string "," Term.to_string vs)
    symb_rp.str

let parse st =
  ( Sl_heap.parse >>= (fun h ->
          parse_symb symb_ind_implies >>
          Inds.parse << spaces >>=
          (fun (_, head) -> return (mk h head))) <?> "case") st

