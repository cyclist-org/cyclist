open Lib
open Util
open Symbols
open MParser

module TPred = PairTypes(Int.T)(Sl_pred)
include TPred

let subst theta (tag, pred) = (tag, Sl_pred.subst theta pred)

let subst_tag tagpairs (tag, pred) =
  let (_, tag'') = TagPairs.find (fun (tag',_) -> tag=tag') tagpairs in
  (tag'', pred) 

let unify ?(tagpairs=false) ?(sub_check=Sl_term.trivial_sub_check)
    ?(cont=Sl_term.trivial_continuation) ?(init_state=Sl_term.empty_state)
    (tag, pred) (tag', pred') = 
  Sl_pred.unify 
    ~sub_check 
    ~cont:(fun (theta, tps) -> 
      cont (theta, if tagpairs then TagPairs.add (tag,tag') tps else tps))
    ~init_state pred pred'

let predsym tpred = Sl_pred.predsym (snd tpred)
let args tpred = Sl_pred.args (snd tpred)
let arity tpred = Sl_pred.arity (snd tpred)

let terms tpred = Sl_pred.terms (snd tpred)
let vars tpred = Sl_term.filter_vars (terms tpred)

let to_string (tag, (pred, args)) =
  (Sl_predsym.to_string pred) ^ symb_caret.str ^
  (string_of_int tag) ^ symb_lp.str ^ 
  (Sl_term.FList.to_string_sep symb_comma.sep args) ^ 
  symb_rp.str
  
let to_melt (t,(ident,args)) =
  Latex.concat
    ([ Latex.index
        (Sl_predsym.to_melt ident)
        (Latex.text (string_of_int t));
      symb_lp.melt; ltx_comma (Blist.map Sl_term.to_melt args); symb_rp.melt ])
      
let parse st =
  (Sl_predsym.parse >>= (fun pred ->
  option parse_tag >>= (fun opt_tag ->
  Tokens.parens (Tokens.comma_sep Sl_term.parse) << spaces >>= (fun arg_list ->
  let tag = match opt_tag with
  | Some tag -> upd_tag tag 
  | None -> next_tag () in
  return (tag, (pred, arg_list))))) <?> "ind") st

let norm eqs (t, pred) = (t, Sl_pred.norm eqs pred)
