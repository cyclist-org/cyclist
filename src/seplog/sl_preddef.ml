open Lib
open Util
open Symbols
open MParser

include PairTypes(MakeFList(Sl_indrule))(Strng)

let dest d = d
let predsym = snd
let rules = fst 

let pp fmt (rules, predsym) =
  Format.fprintf fmt "@[<v 2>%s%s@.%a@.%s@]" predsym symb_lb.sep
    (Blist.pp 
      (fun fmt () -> Format.fprintf fmt "%s@." symb_ind_sep.sep) 
      Sl_indrule.pp) rules
  symb_rb.str
  
let to_string (rules, predsym) =
  predsym ^ symb_lb.sep ^ "\n" ^
  (Blist.to_string ((symb_ind_sep.sep) ^ "\n") Sl_indrule.to_string rules)
  ^ "\n" ^ symb_rb.str

let mk ((rules, predsym) as def) = 
  assert (rules<>[]) ;
  let a = Sl_indrule.arity (Blist.hd rules) in
  assert (Blist.for_all (fun r -> a = Sl_indrule.arity r) rules) ;
  assert (Blist.for_all (fun r -> Strng.equal predsym (Sl_indrule.predsym r)) rules) ;
  def

let parse st =
  (spaces >> parse_ident >>= (fun name ->
    Tokens.braces (sep_by1 Sl_indrule.parse (parse_symb symb_ind_sep)) <<
    spaces >>= (fun cases -> return (mk (cases, name)))) <?> "preddef") st
