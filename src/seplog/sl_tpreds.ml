open Lib
open Symbols
open MParser
include Multiset.Make (Sl_tpred)

let subst theta elts = endomap (Sl_tpred.subst theta) elts

let terms inds =
  Sl_term.Set.union_of_list (Blist.map Sl_tpred.terms (elements inds))

let vars inds = Sl_term.filter_vars (terms inds)

let idents inds =
  map_to Sl_predsym.MSet.add Sl_predsym.MSet.empty
    (fun (_, (id, _)) -> id)
    inds

let to_string_list v = Blist.map Sl_tpred.to_string (elements v)

let to_string v = Blist.to_string symb_star.sep Sl_tpred.to_string (elements v)

let tags inds =
  Tags.of_list
    (Option.list_get
       (Blist.map
          (fun p -> Option.mk (Sl_tpred.is_tagged p) (fst p))
          (to_list inds)))

let strip_tags inds = map_to Sl_pred.MSet.add Sl_pred.MSet.empty snd inds

let equal_upto_tags inds inds' =
  Sl_pred.MSet.equal (strip_tags inds) (strip_tags inds')

let subst_tags tagpairs inds = endomap (Sl_tpred.subst_tag tagpairs) inds

let freshen_tags inds' inds =
  if is_empty inds || is_empty inds' then inds
  else
    let ts, ts' = Pair.map tags (inds, inds') in
    let clashing = Tags.inter ts ts' in
    if Tags.is_empty clashing then inds
    else
      let all_tags = Tags.union ts ts' in
      let free, ex = Tags.partition Tags.is_free_var clashing in
      let subst =
        Tagpairs.union
          (Tagpairs.mk_free_subst all_tags free)
          (Tagpairs.mk_ex_subst all_tags ex)
      in
      endomap (fun (tag, head) -> (Tagpairs.apply_to_tag subst tag, head)) inds

let unify ?(total = true) ?(tagpairs = true) ?(update_check = Fun._true) inds
    inds' cont init_state =
  mk_unifier total true
    (Sl_tpred.unify ~tagpairs ~update_check)
    inds inds' cont init_state

let biunify ?(total = true) ?(tagpairs = true) ?(update_check = Fun._true) inds
    inds' cont init_state =
  mk_unifier total true
    (Sl_tpred.biunify ~tagpairs ~update_check)
    inds inds' cont init_state

let subsumed_upto_tags ?(total = true) eqs inds inds' =
  let rec aux uinds uinds' =
    if Sl_pred.MSet.is_empty uinds then
      (not total) || Sl_pred.MSet.is_empty uinds'
    else
      let uind = Sl_pred.MSet.choose uinds in
      let uinds = Sl_pred.MSet.remove uind uinds in
      let uind = Sl_pred.norm eqs uind in
      match
        Sl_pred.MSet.find_opt
          (fun uind' -> Sl_pred.equal uind (Sl_pred.norm eqs uind'))
          uinds'
      with
      | None -> false
      | Some uind' -> aux uinds (Sl_pred.MSet.remove uind' uinds')
  in
  let uinds, uinds' = Pair.map strip_tags (inds, inds') in
  aux uinds uinds'

let rec subsumed ?(total = true) eqs inds inds' =
  if is_empty inds then (not total) || is_empty inds'
  else
    let ind = choose inds in
    let inds = remove ind inds in
    let ind = Sl_tpred.norm eqs ind in
    match
      find_opt (fun ind' -> Sl_tpred.equal ind (Sl_tpred.norm eqs ind')) inds'
    with
    | None -> false
    | Some ind' -> subsumed ~total eqs inds (remove ind' inds')

let norm eqs inds = endomap (Sl_tpred.norm eqs) inds
