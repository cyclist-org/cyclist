open Lib
open   Symbols
open Generic

open MParser

type t = Prod.t * (Term.t list)

let mk p pa = (p,pa)
let dest c = c
let vars (f,vs) =
  Term.filter_vars (Term.Set.union (Term.Set.of_list vs) (Prod.terms f))
let subst theta (f, vs) =
  (Prod.subst theta f, Term.subst_list theta vs)

let to_string ident (f, params) =
  (Prod.to_string f) ^ " => " ^ ident ^
  (bracket (Blist.to_string "," Term.to_string params))


(*   | prod = product; IND_IMPLIES; pred = IDENT; ts = terms  *)
(* { (FO.Case.mk prod ts, pred) }                             *)
let parse_case st =
  ( Prod.parse >>= (fun p ->
    parse_symb symb_ind_implies >>
    parse_ident >>= (fun pred ->
    Tokens.parens (sep_by1 Term.parse (parse_symb symb_comma)) <<
    spaces >>= (fun ts ->
    return (mk p ts, pred)))) <?> "Case") st

(*   | LB; inds = separated_nonempty_list(IND_SEP, ind_case); RB  *)
(* { inds }                                                       *)
let parse_cases st =
  ( parse_symb symb_lb >>
    sep_by1 parse_case (parse_symb symb_ind_sep) <<
    parse_symb symb_rb <?> "Cases") st

let freshen varset case =
  let casevars = vars case in
  let allvars = Term.Set.union varset casevars in
  let (exist_vars, univ_vars) =
    Pair.map
      Term.Set.elements (Term.Set.partition Term.is_exist_var casevars) in
  let fresh_u_vars = Term.fresh_fvars allvars (Blist.length univ_vars) in
  let fresh_e_vars = Term.fresh_evars allvars (Blist.length exist_vars) in
  let theta = Term.Map.of_list
    ((Blist.combine univ_vars fresh_u_vars) @
     (Blist.combine exist_vars fresh_e_vars)) in
  subst theta case
