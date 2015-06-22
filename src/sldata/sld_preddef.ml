open Lib
open Util
open Symbols
open MParser

include PairTypes(MakeFList(Sld_indrule))(Sld_predsym)

let dest d = d
let predsym = snd
let rules = fst 

let pp fmt (rules, predsym) =
  Format.fprintf fmt "@[<v 2>%a%s@.%a@.%s@]" 
    Sld_predsym.pp predsym 
    symb_lb.sep
    (Blist.pp 
      (fun fmt () -> Format.fprintf fmt "%s@." symb_ind_sep.sep) 
      Sld_indrule.pp) rules
  symb_rb.str
  
let to_string (rules, predsym) =
  (Sld_predsym.to_string predsym) ^ symb_lb.sep ^ "\n" ^
  (Blist.to_string ((symb_ind_sep.sep) ^ "\n") Sld_indrule.to_string rules)
  ^ "\n" ^ symb_rb.str

let mk ((rules, predsym) as def) = 
  assert (rules<>[]) ;
  let a = Sld_indrule.arity (Blist.hd rules) in
  assert (Blist.for_all (fun r -> a = Sld_indrule.arity r) rules) ;
  assert (Blist.for_all (fun r -> Sld_predsym.equal predsym (Sld_indrule.predsym r)) rules) ;
  def

let parse st =
  (spaces >> Sld_predsym.parse >>= (fun name ->
    Tokens.braces (sep_by1 Sld_indrule.parse (parse_symb symb_ind_sep)) <<
    spaces >>= (fun cases -> return (mk (cases, name)))) <?> "preddef") st
