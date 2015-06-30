open Lib
open Util
open Symbols
open MParser

include PairTypes(MakeFList(Sl_indrule))(Sl_predsym)

let dest d = d
let predsym = snd
let rules = fst 

let pp fmt (rules, predsym) =
  Format.fprintf fmt "@[<v 2>%a%s@.%a@.%s@]" 
    Sl_predsym.pp predsym 
    symb_lb.sep
    (Blist.pp 
      (fun fmt () -> Format.fprintf fmt "%s@." symb_ind_sep.sep) 
      Sl_indrule.pp) rules
  symb_rb.str
  
let to_string (rules, predsym) =
  (Sl_predsym.to_string predsym) ^ symb_lb.sep ^ "\n" ^
  (Blist.to_string ((symb_ind_sep.sep) ^ "\n") Sl_indrule.to_string rules)
  ^ "\n" ^ symb_rb.str

let mk ((rules, predsym) as def) = 
  assert (rules<>[]) ;
  let a = Sl_indrule.arity (Blist.hd rules) in
  assert (Blist.for_all (fun r -> a = Sl_indrule.arity r) rules) ;
  assert (Blist.for_all (fun r -> Sl_predsym.equal predsym (Sl_indrule.predsym r)) rules) ;
  def

let parse st =
  (spaces >> Sl_predsym.parse >>= (fun name ->
    Tokens.braces (sep_by1 Sl_indrule.parse (parse_symb symb_ind_sep)) <<
    spaces >>= (fun cases -> return (mk (cases, name)))) <?> "preddef") st

let memory_consuming (rules, _) = 
  Blist.for_all Sl_indrule.memory_consuming rules

let constructively_valued (rules, _) = 
  Blist.for_all Sl_indrule.constructively_valued rules

  
let deterministic (rules, _) =
  let arity = Sl_indrule.arity (Blist.hd rules) in
  let allvars = 
    Sl_term.Set.union_of_list (Blist.map Sl_indrule.vars rules) in
  let newformals = Sl_term.fresh_uvars allvars arity in
  let freshen_rule r = 
    let formals = Sl_indrule.formals r in
    let theta = Sl_term.Map.of_list (Blist.combine formals newformals) in
    Sl_indrule.subst theta r in 
  let freshrules = Blist.map freshen_rule rules in
  let bodies = Blist.map (fun r -> Sl_indrule.body r) freshrules in
  let projbodies = Blist.map (fun b -> Sl_heap.project b newformals) bodies in
  let rec aux = function
    | [] -> true
    | b::bs -> 
      Blist.for_all 
        (fun b' -> Sl_heap.inconsistent (Sl_heap.star b b')) 
        bs 
      && 
      aux bs in
  aux projbodies  