open Lib
open Util
open Symbols
open MParser

include MakeMultiset(Sl_pto)

let subst theta ptos = endomap (Sl_pto.subst theta) ptos

let to_string_list v = Blist.map Sl_pto.to_string (elements v)
let to_string v =
  Blist.to_string symb_star.sep Sl_pto.to_string (elements v)
let to_melt v =
  ltx_star (Blist.map Sl_pto.to_melt (elements v))

let terms ptos =
  Sl_term.Set.union_of_list (Blist.map Sl_pto.terms (elements ptos)) 

let vars p = Sl_term.filter_vars (terms p)

let parse st =
  (Sl_term.parse >>= (fun x ->
          parse_symb symb_pointsto >>
          Tokens.comma_sep1 Sl_term.parse << spaces |>>
          (fun l -> (x, l))) <?> "pto") st

let rec aux_unify part cont theta ptos ptos' =
  if is_empty ptos then
    if part || is_empty ptos' then cont theta else None
  else
    let a = choose ptos in
    let ptos = remove a ptos in
    let to_match = elements ptos' in
    let f a' =
      Option.bind 
        (fun theta' -> aux_unify part cont theta' ptos (remove a' ptos'))
        (Sl_pto.unify theta a a') in
    Blist.find_some f to_match

let unify cont theta ptos ptos' = aux_unify false cont theta ptos ptos'
let unify_with_part cont theta ptos ptos' = aux_unify true cont theta ptos ptos'

let subsumed eqs ptos ptos' =
  match unify (Sl_uf.subst_subsumed eqs) Sl_term.empty_subst ptos ptos' with
  | None -> false
  | Some theta -> 
    assert (equal (subst theta ptos) (subst theta ptos')) ; true

