open Lib
open Util
open Symbols
open MParser

include MakeMultiset(Sld_pto)

let subst theta ptos = endomap (Sld_pto.subst theta) ptos

let to_string_list v = Blist.map Sld_pto.to_string (elements v)
let to_string v =
  Blist.to_string symb_star.sep Sld_pto.to_string (elements v)
let to_melt v =
  ltx_star (Blist.map Sld_pto.to_melt (elements v))

let terms ptos =
  Sld_term.Set.union_of_list (Blist.map Sld_pto.terms (elements ptos)) 

let vars p = Sld_term.filter_vars (terms p)

let parse st =
  (Sld_term.parse >>= (fun x ->
          parse_symb symb_pointsto >>
          Tokens.comma_sep1 Sld_term.parse << spaces |>>
          (fun l -> (x, l))) <?> "pto") st

let rec unify ?(total=true) 
    ?(sub_check=Sld_term.trivial_sub_check)
    ?(cont=Sld_term.trivial_continuation)
    ?(init_state=Sld_term.empty_state) ptos ptos' =
  if is_empty ptos then
    if not total || is_empty ptos' then cont init_state else None
  else
    let a = choose ptos in
    let ptos = remove a ptos in
    let f a' =
      Sld_pto.unify ~sub_check  
        ~cont:(fun state' -> 
          unify ~total ~sub_check ~cont ~init_state:state' ptos (remove a' ptos'))
        ~init_state a a' in
    find_map f ptos'

let rec subsumed ?(total=true) eqs ptos ptos' =
  if is_empty ptos then not total || is_empty ptos' else
  let pto = choose ptos in
  let ptos = remove pto ptos in
  let pto = Sld_pto.norm eqs pto in
  match find_opt (fun pto' -> Sld_pto.equal pto (Sld_pto.norm eqs pto')) ptos' with
  | None -> false
  | Some pto' -> subsumed ~total eqs ptos (remove pto' ptos')

let norm eqs ptos = endomap (Sld_pto.norm eqs) ptos
