open Lib
open Util
open Symbols
open MParser

include MakeMultiset(Sl_tpred)

let subst theta elts = endomap (Sl_tpred.subst theta) elts

let terms inds =
  Sl_term.Set.union_of_list (Blist.map Sl_tpred.terms  (elements inds))

let vars inds = Sl_term.filter_vars (terms inds)

let to_string_list v = Blist.map Sl_tpred.to_string (elements v)
let to_string v =
  Blist.to_string symb_star.sep Sl_tpred.to_string (elements v)
let to_melt v =
  ltx_star (Blist.map Sl_tpred.to_melt (elements v))


let tags inds = map_to Tags.add Tags.empty fst inds

let freshen_tags inds' inds =
  if is_empty inds || is_empty inds' then inds else
    let maxtag = Tags.max_elt (tags inds') in
    let mintag = fold (fun (tag, _) a -> min tag a) inds max_int in
    let delta = 1 + maxtag - mintag in
    endomap (fun (tag, head) -> (tag + delta, head)) inds

let subsumed eqs inds inds' =
  let theta = Sl_uf.to_subst eqs in
  equal (subst theta inds) (subst theta inds')

let rec aux_unify part cont theta inds inds' =
  if is_empty inds then
    if part || is_empty inds' then cont theta else None
  else
    let a = choose inds in
    let inds = remove a inds in
    let to_match = elements inds' in
    let f a' =
      Option.bind 
        (fun theta' -> aux_unify part cont theta' inds (remove a' inds'))
        (Sl_tpred.unify theta a a') in
    Blist.find_some f to_match

let unify cont theta inds inds' = aux_unify false cont theta inds inds'
let part_unify cont theta inds inds' = aux_unify true cont theta inds inds'
