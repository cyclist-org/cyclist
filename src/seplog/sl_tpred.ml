open Lib
open Util
open Symbols
open MParser

module TPred = PairTypes(Int.T)(Sl_pred)
include TPred

let subst theta (tag, pred) = (tag, Sl_pred.subst theta pred)
let unify theta (_, pred) (_, pred') = Sl_pred.unify theta pred pred'

let terms (_, pred) = Sl_pred.terms pred
let vars tpred = Sl_term.filter_vars (terms tpred)

let to_string (tag, (pred, args)) =
  pred ^ symb_caret.str ^
  (string_of_int tag) ^ symb_lp.str ^ 
  (Sl_term.FList.to_string_sep symb_comma.sep args) ^ 
  symb_rp.str
  
let to_melt (t,(ident,args)) =
  Latex.concat
    ([ Latex.index
        (Latex.mathit (Latex.text ident))
        (Latex.text (string_of_int t));
      symb_lp.melt; ltx_comma (Blist.map Sl_term.to_melt args); symb_rp.melt ])
      
let parse st =
  (parse_ident >>= (fun pred ->
  option parse_tag >>= (fun opt_tag ->
  Tokens.parens (Tokens.comma_sep1 Sl_term.parse) << spaces >>= (fun arg_list ->
  let tag = match opt_tag with
  | Some tag -> upd_tag tag 
  | None -> next_tag () in
  return (tag, (pred, arg_list))))) <?> "ind") st

 