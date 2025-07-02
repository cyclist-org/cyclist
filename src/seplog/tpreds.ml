open Lib
open   Symbols

open Generic

open MParser

include Multiset.Make (Tpred)

let subst theta elts = map (Tpred.subst theta) elts

let terms inds =
  Term.Set.union_of_list (Blist.map Tpred.terms (elements inds))

let vars inds = Term.filter_vars (terms inds)

let idents inds =
  map_to Predsym.MSet.add Predsym.MSet.empty
    (fun (_, (id, _)) -> id)
    inds

let to_string_list v = Blist.map Tpred.to_string (elements v)

let to_string v = Blist.to_string symb_star.sep Tpred.to_string (elements v)

let tags inds =
  Tags.of_list
    (Option.list_get
       (Blist.map
          (fun p -> Option.mk (Tpred.is_tagged p) (fst p))
          (to_list inds)))

let strip_tags inds = map_to Pred.MSet.add Pred.MSet.empty snd inds

let equal_upto_tags inds inds' =
  Pred.MSet.equal (strip_tags inds) (strip_tags inds')

let subst_tags tagpairs inds = map (Tpred.subst_tag tagpairs) inds

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
      map (fun (tag, head) -> (Tagpairs.apply_to_tag subst tag, head)) inds

let unify ?(total = true) ?(tagpairs = true) ?(update_check = Fun._true) inds
    inds' cont init_state =
  mk_unifier total true
    (Tpred.unify ~tagpairs ~update_check)
    inds inds' cont init_state

let biunify ?(total = true) ?(tagpairs = true) ?(update_check = Fun._true) inds
    inds' cont init_state =
  mk_unifier total true
    (Tpred.biunify ~tagpairs ~update_check)
    inds inds' cont init_state

let subsumed_upto_tags ?(total = true) eqs inds inds' =
  let rec aux uinds uinds' =
    if Pred.MSet.is_empty uinds then
      (not total) || Pred.MSet.is_empty uinds'
    else
      let uind = Pred.MSet.choose uinds in
      let uinds = Pred.MSet.remove uind uinds in
      let uind = Pred.norm eqs uind in
      match
        Pred.MSet.find_suchthat_opt
          (fun uind' -> Pred.equal uind (Pred.norm eqs uind'))
          uinds'
      with
      | None -> false
      | Some uind' -> aux uinds (Pred.MSet.remove uind' uinds')
  in
  let uinds, uinds' = Pair.map strip_tags (inds, inds') in
  aux uinds uinds'

let rec subsumed ?(total = true) eqs inds inds' =
  if is_empty inds then (not total) || is_empty inds'
  else
    let ind = choose inds in
    let inds = remove ind inds in
    let ind = Tpred.norm eqs ind in
    match
      find_suchthat_opt (fun ind' -> Tpred.equal ind (Tpred.norm eqs ind')) inds'
    with
    | None -> false
    | Some ind' -> subsumed ~total eqs inds (remove ind' inds')

let norm eqs inds = map (Tpred.norm eqs) inds
