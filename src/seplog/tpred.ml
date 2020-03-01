open Lib
open   Symbols

open Generic

open MParser

module TPred = Pair.Make (Tags.Elt) (Pred)
include TPred

let subst theta (tag, pred) = (tag, Pred.subst theta pred)

let equal_upto_tags (_, p1) (_, p2) = Pred.equal p1 p2

let subst_tag tagpairs (tag, pred) = (Tagpairs.apply_to_tag tagpairs tag, pred)

let unify ?(tagpairs = false) ?(update_check = Fun._true) (t, pred) (t', pred')
    cont init_state =
  Pred.unify ~update_check pred pred'
    ( if tagpairs then Unify.Unidirectional.unify_tag ~update_check t t' cont
    else cont )
    init_state

let biunify ?(tagpairs = false) ?(update_check = Fun._true) (t, pred)
    (t', pred') cont init_state =
  Pred.biunify ~update_check pred pred'
    ( if tagpairs then Unify.Bidirectional.unify_tag ~update_check t t' cont
    else cont )
    init_state

let predsym tpred = Pred.predsym (snd tpred)

let args tpred = Pred.args (snd tpred)

let arity tpred = Pred.arity (snd tpred)

let tag (t, _) = t

let tags (tag, _) = Tags.singleton tag

let terms tpred = Pred.terms (snd tpred)

let vars tpred = Term.filter_vars (terms tpred)

let tag_is_free (x, _) = Tags.is_free_var x

let tag_is_exist (x, _) = Tags.is_exist_var x

let is_tagged (x, _) = not (Tags.is_anonymous x)

let to_string (tag, (pred, args)) =
  Predsym.to_string pred
  ^ (if Tags.is_anonymous tag then "" else sqbracket (Tags.Elt.to_string tag))
  ^ bracket (Term.FList.to_string_sep symb_comma.sep args)

let parse ?(allow_tags = true) st =
  ( Predsym.parse
  >>= (fun pred ->
        (if allow_tags then option Tags.Elt.parse else return None)
        >>= fun opt_tag ->
        Tokens.parens (Tokens.comma_sep Term.parse)
        << spaces
        >>= fun arg_list ->
        let tag = Option.dest Tags.anonymous Fun.id opt_tag in
        return (tag, (pred, arg_list)) )
  <?> "ind" )
    st

let norm eqs (t, pred) = (t, Pred.norm eqs pred)

let of_string = mk_of_string parse

let pp fmt (tag, pred) =
  Format.fprintf fmt "@[%a%s%s%s%s@]" Predsym.pp (Pred.predsym pred)
    (if Tags.is_anonymous tag then "" else sqbracket (Tags.Elt.to_string tag))
    symb_lp.str
    (Term.FList.to_string_sep symb_comma.sep (Pred.args pred))
    symb_rp.str
