open Lib
open Util
open Symbols
open MParser

include PairTypes(Sld_term)(Sld_term.FList)

let subst theta (lv, rvs) =
  (Sld_term.subst theta lv, Sld_term.FList.subst theta rvs)

let unify ?(sub_check=Sld_term.trivial_sub_check)
    ?(cont=Sld_term.trivial_continuation)
    ?(init_state=Sld_term.empty_state) (x, xs) (y, ys) =
  Sld_term.FList.unify ~sub_check ~cont ~init_state (x::xs) (y::ys)

let to_string (x,args) =
  (Sld_term.to_string x) ^ symb_pointsto.str ^ 
  (Sld_term.FList.to_string_sep symb_comma.sep args)
  
let to_melt (x,args) =
  Latex.concat
    [ Sld_term.to_melt x; symb_pointsto.melt;
    ltx_comma (Blist.map Sld_term.to_melt args) ]

let terms (x, xs) = Sld_term.FList.terms (x::xs)

let vars pto = Sld_term.filter_vars (terms pto) 

let norm eqs (x, xs) = 
  (Sld_uf.find x eqs, Blist.map (fun y -> Sld_uf.find y eqs) xs)

let record_type (x, xs) = (x, Blist.length xs)
