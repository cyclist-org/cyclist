open Lib
open Util
open Symbols
open MParser

module TPred = PairTypes(Int.T)(Sld_pred)
include TPred

let subst theta (tag, pred) = (tag, Sld_pred.subst theta pred)

let subst_tag tagpairs (tag, pred) =
  let (_, tag'') = TagPairs.find (fun (tag',_) -> tag=tag') tagpairs in
  (tag'', pred) 

let unify ?(tagpairs=false) ?(sub_check=Sld_term.trivial_sub_check)
    ?(cont=Sld_term.trivial_continuation) ?(init_state=Sld_term.empty_state)
    (tag, pred) (tag', pred') = 
  Sld_pred.unify 
    ~sub_check 
    ~cont:(fun (theta, tps) -> 
      cont (theta, if tagpairs then TagPairs.add (tag,tag') tps else tps))
    ~init_state pred pred'

let predsym tpred = Sld_pred.predsym (snd tpred)
let args tpred = Sld_pred.args (snd tpred)
let arity tpred = Sld_pred.arity (snd tpred)

let terms tpred = Sld_pred.terms (snd tpred)
let vars tpred = Sld_term.filter_vars (terms tpred)

let to_string (tag, (pred, args)) =
  (Sld_predsym.to_string pred) ^ symb_caret.str ^
  (string_of_int tag) ^ symb_lp.str ^ 
  (Sld_term.FList.to_string_sep symb_comma.sep args) ^ 
  symb_rp.str
  
let to_melt (t,(ident,args)) =
  Latex.concat
    ([ Latex.index
        (Sld_predsym.to_melt ident)
        (Latex.text (string_of_int t));
      symb_lp.melt; ltx_comma (Blist.map Sld_term.to_melt args); symb_rp.melt ])
      
let parse st =
  (Sld_predsym.parse >>= (fun pred ->
  option parse_tag >>= (fun opt_tag ->
  Tokens.parens (Tokens.comma_sep Sld_term.parse) << spaces >>= (fun arg_list ->
  let tag = match opt_tag with
  | Some tag -> upd_tag tag 
  | None -> next_tag () in
  return (tag, (pred, arg_list))))) <?> "ind") st

let norm eqs (t, pred) = (t, Sld_pred.norm eqs pred)
