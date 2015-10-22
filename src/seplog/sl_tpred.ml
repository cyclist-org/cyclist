open Lib
open Util
open Symbols
open MParser

module TPred = PairTypes(Int)(Sl_pred)
include TPred

let subst theta (tag, pred) = (tag, Sl_pred.subst theta pred)

let subst_tag tagpairs (tag, pred) =
  (TagPairs.apply_to_tag tagpairs tag, pred)

let unify ?(tagpairs=false) ?(update_check=Fun._true)
    (tag, pred) (tag', pred') cont init_state =
  let tp = (tag, tag') in
  if (not tagpairs) || 
      (update_check (init_state, (Sl_term.Map.empty, TagPairs.singleton tp))) 
  then
    Sl_pred.unify ~update_check pred pred'
      (fun (theta, tps) ->
        cont (theta, if tagpairs then TagPairs.add tp tps else tps))
      init_state
  else None

let predsym tpred = Sl_pred.predsym (snd tpred)
let tag (t, _) = t
let args tpred = Sl_pred.args (snd tpred)
let arity tpred = Sl_pred.arity (snd tpred)

let terms tpred = Sl_pred.terms (snd tpred)
let vars tpred = Sl_term.filter_vars (terms tpred)

let tag_is_univ (x, _) = Util.Tags.is_univ_var x
let tag_is_exist (x, _) = Util.Tags.is_exist_var x
let is_tagged (x, _) = Util.Tags.is_var x

let to_string (tag, (pred, args)) =
  (Sl_predsym.to_string pred) ^
  (tag_to_string tag) ^
  (bracket (Sl_term.FList.to_string_sep symb_comma.sep args))

let to_melt (tag, (ident,args)) =
  Latex.concat
    ([ Latex.index
        (Sl_predsym.to_melt ident)
        (tag_to_melt tag);
      symb_lp.melt; ltx_comma (Blist.map Sl_term.to_melt args); symb_rp.melt ])

let parse ?(allow_tags=true) st =
  (Sl_predsym.parse >>= (fun pred ->
  (if allow_tags then option parse_tag else (return None)) >>= (fun opt_tag ->
  Tokens.parens (Tokens.comma_sep Sl_term.parse) << spaces >>= (fun arg_list ->
  let tag = Option.dest Util.Tags.anonymous_tag Fun.id opt_tag in
  return (tag, (pred, arg_list))))) <?> "ind") st

let norm eqs (t, pred) = (t, Sl_pred.norm eqs pred)
