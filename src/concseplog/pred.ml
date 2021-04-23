open Lib
open Symbols
open MParser

module IndSubf = struct
  include Pair.Make (Predsym) (Term.FList)

  let to_string (p, args) =
    Predsym.to_string p ^ symb_lp.str
    ^ Term.FList.to_string_sep symb_comma.sep args
    ^ symb_rp.str

  let pp fmt (p, args) =
    Format.fprintf fmt "@[%a%s%s%s@]" Predsym.pp p symb_lp.str
      (Term.FList.to_string_sep symb_comma.sep args)
      symb_rp.str
end

include IndSubf

let unify ?(update_check = Fun._true) (p, args) (p', args') cont init_state =
  if not (Predsym.equal p p') then None
  else
    Unify.Unidirectional.unify_trm_list ~update_check args args' cont
      init_state

let biunify ?(update_check = Fun._true) (p, args) (p', args') cont init_state =
  if not (Predsym.equal p p') then None
  else
    Unify.Bidirectional.unify_trm_list ~update_check args args' cont
      init_state

let predsym pred = fst pred

let args pred = snd pred

let arity (_, args) = Blist.length args

let terms pred = Term.Set.of_list (args pred)

let vars pred = Term.filter_vars (terms pred)

let subst theta (p, args) = (p, Term.FList.subst theta args)

let parse st =
  ( Predsym.parse
  >>= (fun pred ->
        Tokens.parens (Tokens.comma_sep Term.parse)
        << spaces
        >>= fun arg_list -> return (pred, arg_list) )
  <?> "ind" )
    st

let of_string s = handle_reply (MParser.parse_string parse s ())

let norm eqs (ident, args) = (ident, Blist.map (fun x -> Uf.find x eqs) args)

module MSet = Multiset.Make (IndSubf)
