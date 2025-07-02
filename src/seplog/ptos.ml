open Lib
open Symbols
open MParser
include Multiset.Make (Pto)

let subst theta ptos = map (Pto.subst theta) ptos

let to_string_list v = Blist.map Pto.to_string (elements v)

let to_string v = Blist.to_string symb_star.sep Pto.to_string (elements v)

let terms ptos =
  Term.Set.union_of_list (Blist.map Pto.terms (elements ptos))

let vars p = Term.filter_vars (terms p)

let parse st =
  ( Term.parse
  >>= (fun x ->
        parse_symb symb_pointsto
        >> Tokens.comma_sep1 Term.parse
        << spaces
        |>> fun l -> (x, l) )
  <?> "pto" )
    st

let rec unify ?(total = true) ?(update_check = Fun._true) ptos ptos' cont
    init_state =
  mk_unifier total true (Pto.unify ~update_check) ptos ptos' cont init_state

let rec biunify ?(total = true) ?(update_check = Fun._true) ptos ptos' cont
    init_state =
  mk_unifier total true
    (Pto.biunify ~update_check)
    ptos ptos' cont init_state

let rec subsumed ?(total = true) eqs ptos ptos' =
  if is_empty ptos then (not total) || is_empty ptos'
  else
    let pto = choose ptos in
    let ptos = remove pto ptos in
    let pto = Pto.norm eqs pto in
    match
      find_suchthat_opt (fun pto' -> Pto.equal pto (Pto.norm eqs pto')) ptos'
    with
    | None -> false
    | Some pto' -> subsumed ~total eqs ptos (remove pto' ptos')

let norm eqs ptos = map (Pto.norm eqs) ptos
