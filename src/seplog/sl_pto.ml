open Lib
open Util
open Symbols
open MParser

include PairTypes(Sl_term)(Sl_term.FList)

let subst theta (lv, rvs) =
  (Sl_term.subst theta lv, Sl_term.FList.subst theta rvs)

let unify ?(sub_check=Sl_term.trivial_sub_check)
    ?(cont=Sl_term.trivial_continuation)
    ?(init_state=Sl_term.empty_state) (x, xs) (y, ys) =
  Sl_term.FList.unify ~sub_check ~cont ~init_state (x::xs) (y::ys)

let to_string (x,args) =
  (Sl_term.to_string x) ^ symb_pointsto.str ^ 
  (Sl_term.FList.to_string_sep symb_comma.sep args)
  
let to_melt (x,args) =
  Latex.concat
    [ Sl_term.to_melt x; symb_pointsto.melt;
    ltx_comma (Blist.map Sl_term.to_melt args) ]

let terms (x, xs) = Sl_term.FList.terms (x::xs)

let vars pto = Sl_term.filter_vars (terms pto) 

let norm eqs (x, xs) = 
  (Sl_uf.find x eqs, Blist.map (fun y -> Sl_uf.find y eqs) xs)

let record_type (x, xs) = (x, Blist.length xs)

let parse st =
  (Sl_term.parse >>= (fun x ->
          parse_symb symb_pointsto >>
          Tokens.comma_sep1 Sl_term.parse << spaces |>>
          (fun l -> (x, l))) <?> "pto") st
