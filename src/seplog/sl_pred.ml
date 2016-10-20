open Lib

open Symbols
open MParser

module IndSubf = 
  struct
    include Pair.Make(Sl_predsym)(Sl_term.FList)
    
    let to_string (p, args) = 
      (Sl_predsym.to_string p) ^ symb_lp.str ^ 
      (Sl_term.FList.to_string_sep symb_comma.sep args) ^ 
      symb_rp.str

    let pp fmt (p, args) =
      Format.fprintf fmt "@[%a%s%s%s@]"
        Sl_predsym.pp p 
        symb_lp.str 
        (Sl_term.FList.to_string_sep symb_comma.sep args) 
        symb_rp.str
end

include IndSubf

let unify ?(update_check=Fun._true)
    (p, args) (p', args') cont init_state =
  if not (Sl_predsym.equal p p') then None else
  Sl_unify.Unidirectional.unify_trm_list ~update_check 
    args args' cont init_state

let biunify ?(update_check=Fun._true)
    (p, args) (p', args') cont init_state =
  if not (Sl_predsym.equal p p') then None else
  Sl_unify.Bidirectional.unify_trm_list ~update_check 
    args args' cont init_state

let predsym pred = fst pred
let args pred = snd pred
let arity (_, args) = Blist.length args

let terms pred = Sl_term.Set.of_list (args pred)
let vars pred = Sl_term.filter_vars (terms pred)
let subst theta (p, args) = (p, Sl_term.FList.subst theta args)
let parse st =
  (Sl_predsym.parse >>= (fun pred ->
  Tokens.parens (Tokens.comma_sep Sl_term.parse) << spaces >>= (fun arg_list ->
  return (pred, arg_list))) <?> "ind") st

let of_string s =
  handle_reply (MParser.parse_string parse s ())

let norm eqs (ident, args) = (ident, Blist.map (fun x -> Sl_uf.find x eqs) args)

module MSet = Multiset.Make(IndSubf)