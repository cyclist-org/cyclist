open Lib
open Util
open Symbols
open MParser

include PairTypes(Sl_term)(Sl_term.FList)

let subst theta (lv, rvs) =
  (Sl_term.subst theta lv, Sl_term.FList.subst theta rvs)

let unify cont state (x, xs) (y, ys) =
  Sl_term.FList.unify cont state (x::xs) (y::ys)

let to_string (x,args) =
  (Sl_term.to_string x) ^ symb_pointsto.str ^ 
  (Sl_term.FList.to_string_sep symb_comma.sep args)
  
let to_melt (x,args) =
  Latex.concat
    [ Sl_term.to_melt x; symb_pointsto.melt;
    ltx_comma (Blist.map Sl_term.to_melt args) ]

let terms (x, xs) = Sl_term.FList.terms (x::xs)

let vars pto = Sl_term.filter_vars (terms pto) 