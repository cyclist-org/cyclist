open Lib
open Util
open Symbols
open MParser

include MakeMultiset(Sl_tpred)

let subst theta elts = endomap (Sl_tpred.subst theta) elts

let terms inds =
  Sl_term.Set.union_of_list (Blist.map Sl_tpred.terms  (elements inds))

let vars inds = Sl_term.filter_vars (terms inds)

let idents inds =
  map_to Strng.MSet.add Strng.MSet.empty (fun (_, (id, _)) -> id) inds

let to_string_list v = Blist.map Sl_tpred.to_string (elements v)
let to_string v =
  Blist.to_string symb_star.sep Sl_tpred.to_string (elements v)
let to_melt v =
  ltx_star (Blist.map Sl_tpred.to_melt (elements v))


let tags inds = Tags.of_list (Blist.map fst (to_list inds))

let strip_tags inds = 
  map_to Sl_pred.MSet.add Sl_pred.MSet.empty snd inds

let equal_upto_tags inds inds' =
  Sl_pred.MSet.equal (strip_tags inds) (strip_tags inds')
  
let subst_tags tagpairs inds = 
  endomap (Sl_tpred.subst_tag tagpairs) inds
  

let freshen_tags inds' inds =
  if is_empty inds || is_empty inds' then inds else
    let maxtag = Tags.max_elt (tags inds') in
    let mintag = fold (fun (tag, _) a -> min tag a) inds max_int in
    let delta = 1 + maxtag - mintag in
    endomap (fun (tag, head) -> (tag + delta, head)) inds

let rec unify ?(total=true) ?(tagpairs=false) cont state inds inds' =
  if is_empty inds then
    if not total || is_empty inds' then cont state else None
  else
    let a = choose inds in
    let inds = remove a inds in
    let to_match = elements inds' in
    let f a' =
      Sl_tpred.unify ~tagpairs
        (fun state' -> unify ~total ~tagpairs cont state' inds (remove a' inds'))
        state a a' in
    Blist.find_some f to_match

let subsumed_upto_tags ?(total=true) eqs inds inds' =
  let valid ((theta,_) as state) =
    if 
      not 
        ((if total then Sl_pred.MSet.equal else Sl_pred.MSet.subset) 
          (strip_tags (subst theta inds))
          (strip_tags (subst theta inds'))) then 
      None 
    else
      Sl_uf.subst_subsumed eqs state in
  Option.is_some 
    (unify ~total valid (Sl_term.empty_subst, TagPairs.empty) inds inds')
      
let subsumed ?(total=true) eqs inds inds' =
  let valid ((theta,_) as state) =
    if 
      not 
        ((if total then equal else subset) (subst theta inds) inds') 
    then 
      None 
    else
      Sl_uf.subst_subsumed eqs state in
  Option.is_some 
    (unify ~total valid (Sl_term.empty_subst, TagPairs.empty) inds inds')
      

