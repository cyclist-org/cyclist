open Lib
open Util
open Symbols
open MParser

module IndSubf = 
  struct
    include PairTypes(Sld_predsym)(Sld_term.FList)
    
    let to_string (p, args) = 
      (Sld_predsym.to_string p) ^ symb_lp.str ^ 
      (Sld_term.FList.to_string_sep symb_comma.sep args) ^ 
      symb_rp.str

    let pp fmt (p, args) =
      Format.fprintf fmt "@[%a%s%s%s@]"
        Sld_predsym.pp p 
        symb_lp.str 
        (Sld_term.FList.to_string_sep symb_comma.sep args) 
        symb_rp.str
end

include IndSubf

let unify ?(sub_check=Sld_term.trivial_sub_check)
    ?(cont=Sld_term.trivial_continuation)
    ?(init_state=Sld_term.empty_state) (p, args) (p', args') =
  if not (Sld_predsym.equal p p') then None else
  Sld_term.FList.unify ~sub_check ~cont ~init_state args args'

let predsym pred = fst pred
let args pred = snd pred
let arity (_, args) = Blist.length args

let terms pred = Sld_term.Set.of_list (args pred)
let vars pred = Sld_term.filter_vars (terms pred)
let subst theta (p, args) = (p, Sld_term.FList.subst theta args)
let parse st =
  (Sld_predsym.parse >>= (fun pred ->
  Tokens.parens (Tokens.comma_sep Sld_term.parse) << spaces >>= (fun arg_list ->
  return (pred, arg_list))) <?> "ind") st

let of_string s =
  handle_reply (MParser.parse_string parse s ())

let norm eqs (ident, args) = (ident, Blist.map (fun x -> Sld_uf.find x eqs) args)

module MSet = MakeMultiset(IndSubf)
