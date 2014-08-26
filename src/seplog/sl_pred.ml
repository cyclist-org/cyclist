open Lib
open Util
open Symbols
open MParser

type ident_t = Strng.t

module IndSubf = 
  struct
    include PairTypes(Strng)(Sl_term.FList)
    
    let to_string (p, args) = 
      p ^ symb_lp.str ^ 
      (Sl_term.FList.to_string_sep symb_comma.sep args) ^ 
      symb_rp.str

    let pp fmt (p, args) =
      Format.fprintf fmt "@[%s%s%s%s@]"
        p symb_lp.str 
        (Sl_term.FList.to_string_sep symb_comma.sep args) 
        symb_rp.str
end

include IndSubf

let unify cont state (p, args) (p', args') =
  if not (Strng.equal p p') then None else
  Sl_term.FList.unify cont state args args'

let predsym pred = fst pred
let args pred = snd pred
let arity (_, args) = Blist.length args

let terms pred = Sl_term.Set.of_list (args pred)
let vars pred = Sl_term.filter_vars (terms pred)
let subst theta (p, args) = (p, Sl_term.FList.subst theta args)
let parse st =
  (parse_ident >>= (fun pred ->
  Tokens.parens (Tokens.comma_sep1 Sl_term.parse) << spaces >>= (fun arg_list ->
  return (pred, arg_list))) <?> "ind") st

let of_string s =
  handle_reply (MParser.parse_string parse s ())

module MSet = MakeMultiset(IndSubf)