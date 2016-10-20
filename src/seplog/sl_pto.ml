open Lib

open Symbols
open MParser

include Pair.Make(Sl_term)(Sl_term.FList)

let subst theta (lv, rvs) =
  (Sl_subst.apply theta lv, Sl_term.FList.subst theta rvs)

let unify ?(update_check=Fun._true)
    (x, xs) (y, ys) cont init_state =
  Sl_unify.Unidirectional.unify_trm_list ~update_check 
    (x::xs) (y::ys) cont init_state

let biunify ?(update_check=Fun._true)
    (x, xs) (y, ys) cont init_state =
  Sl_unify.Bidirectional.unify_trm_list ~update_check 
    (x::xs) (y::ys) cont init_state

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

let pp fmt (x,ys) = 
  Format.fprintf fmt "@[%a%s%a@]" 
    Sl_term.pp x symb_pointsto.str (Blist.pp pp_comma Sl_term.pp) ys
  