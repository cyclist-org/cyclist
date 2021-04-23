open Lib
open Symbols
open MParser
include Pair.Make (Term) (Term.FList)

let subst theta (lv, rvs) =
  (Subst.apply theta lv, Term.FList.subst theta rvs)

let unify ?(update_check = Fun._true) (x, xs) (y, ys) cont init_state =
  Unify.Unidirectional.unify_trm_list ~update_check (x :: xs) (y :: ys) cont
    init_state

let biunify ?(update_check = Fun._true) (x, xs) (y, ys) cont init_state =
  Unify.Bidirectional.unify_trm_list ~update_check (x :: xs) (y :: ys) cont
    init_state

let to_string (x, args) =
  Term.to_string x ^ symb_pointsto.str
  ^ Term.FList.to_string_sep symb_comma.sep args

let terms (x, xs) = Term.FList.terms (x :: xs)

let vars pto = Term.filter_vars (terms pto)

let norm eqs (x, xs) =
  (Uf.find x eqs, Blist.map (fun y -> Uf.find y eqs) xs)

let record_type (x, xs) = (x, Blist.length xs)

let parse st =
  ( Term.parse
  >>= (fun x ->
        parse_symb symb_pointsto
        >> Tokens.comma_sep1 Term.parse
        << spaces
        |>> fun l -> (x, l) )
  <?> "pto" )
    st

let pp fmt (x, ys) =
  Format.fprintf fmt "@[%a%s%a@]" Term.pp x symb_pointsto.str
    (Blist.pp pp_comma Term.pp)
    ys
