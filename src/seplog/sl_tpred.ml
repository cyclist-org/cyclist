open Lib

open Symbols
open MParser

module TPred = Pair.Make(Tags.Elt)(Sl_pred)
include TPred

let subst theta (tag, pred) = (tag, Sl_pred.subst theta pred)

let equal_upto_tags (_, p1) (_, p2) = Sl_pred.equal p1 p2

let subst_tag tagpairs (tag, pred) =
  (Tagpairs.apply_to_tag tagpairs tag, pred)

let unify ?(tagpairs=false) ?(update_check=Fun._true)
    (t, pred) (t', pred') cont init_state =
  Sl_pred.unify ~update_check pred pred'
  (if tagpairs
    then Sl_unify.Unidirectional.unify_tag ~update_check t t' cont
    else cont)
  init_state

let biunify ?(tagpairs=false) ?(update_check=Fun._true)
    (t, pred) (t', pred') cont init_state =
  Sl_pred.biunify ~update_check pred pred'
  (if tagpairs
    then Sl_unify.Bidirectional.unify_tag ~update_check t t' cont
    else cont)
  init_state

let predsym tpred = Sl_pred.predsym (snd tpred)
let tag (t, _) = t
let args tpred = Sl_pred.args (snd tpred)
let arity tpred = Sl_pred.arity (snd tpred)
let tag (t, _) = t
let tags (tag, _) = Tags.singleton tag

let terms tpred = Sl_pred.terms (snd tpred)
let vars tpred = Sl_term.filter_vars (terms tpred)

let tag_is_free (x, _) = Tags.is_free_var x
let tag_is_exist (x, _) = Tags.is_exist_var x
let is_tagged (x, _) = not (Tags.is_anonymous x)

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
  let tag = Option.dest Tags.anonymous Fun.id opt_tag in
  return (tag, (pred, arg_list))))) <?> "ind") st

let norm eqs (t, pred) = (t, Sl_pred.norm eqs pred)

let of_string = mk_of_string parse
  
let pp fmt (tag, pred) =
  Format.fprintf fmt "@[%a%s%s%s%s%s@]"
    Sl_predsym.pp (Sl_pred.predsym pred)
    symb_caret.str
    (tag_to_string tag) 
    symb_lp.str 
    (Sl_term.FList.to_string_sep symb_comma.sep (Sl_pred.args pred)) 
    symb_rp.str
  