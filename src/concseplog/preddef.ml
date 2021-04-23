open Lib
open Symbols
open MParser
include Pair.Make (Flist.Make (Indrule)) (Predsym)

let dest d = d

let predsym = snd

let rules = fst

let pp fmt (rules, predsym) =
  Format.fprintf fmt "@[<v 2>%a%s@.%a@.%s@]" Predsym.pp predsym symb_lb.sep
    (Blist.pp
       (fun fmt () -> Format.fprintf fmt "%s@." symb_ind_sep.sep)
       Indrule.pp)
    rules symb_rb.str

let to_string (rules, predsym) =
  Predsym.to_string predsym
  ^ symb_lb.sep ^ "\n"
  ^ Blist.to_string (symb_ind_sep.sep ^ "\n") Indrule.to_string rules
  ^ "\n" ^ symb_rb.str

let mk ((rules, predsym) as def) =
  assert (not (Blist.is_empty rules)) ;
  let a = Indrule.arity (Blist.hd rules) in
  assert (Blist.for_all (fun r -> Int.equal a (Indrule.arity r)) rules) ;
  assert (
    Blist.for_all
      (fun r -> Predsym.equal predsym (Indrule.predsym r))
      rules ) ;
  def

let parse st =
  ( spaces >> Predsym.parse
  >>= (fun name ->
        Tokens.braces (sep_by1 Indrule.parse (parse_symb symb_ind_sep))
        << spaces
        >>= fun cases -> return (mk (cases, name)) )
  <?> "preddef" )
    st

let memory_consuming (rules, _) =
  Blist.for_all Indrule.memory_consuming rules

let constructively_valued (rules, _) =
  Blist.for_all Indrule.constructively_valued rules

let deterministic (rules, _) =
  let arity = Indrule.arity (Blist.hd rules) in
  let allvars = Term.Set.union_of_list (Blist.map Indrule.vars rules) in
  let newformals = Term.fresh_fvars allvars arity in
  let freshen_rule r =
    let formals = Indrule.formals r in
    let theta = Term.Map.of_list (Blist.combine formals newformals) in
    Indrule.subst theta r
  in
  let freshrules = Blist.map freshen_rule rules in
  let bodies = Blist.map (fun r -> Indrule.body r) freshrules in
  let projbodies = Blist.map (fun b -> Heap.project b newformals) bodies in
  let rec aux = function
    | [] -> true
    | b :: bs ->
        Blist.for_all (fun b' -> Heap.inconsistent (Heap.star b b')) bs
        && aux bs
  in
  aux projbodies
