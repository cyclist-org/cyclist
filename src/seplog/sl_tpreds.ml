open Lib

open Symbols
open MParser

include Multiset.Make(Sl_tpred)

let subst theta elts = endomap (Sl_tpred.subst theta) elts

let terms inds =
  Sl_term.Set.union_of_list (Blist.map Sl_tpred.terms  (elements inds))

let vars inds = Sl_term.filter_vars (terms inds)

let idents inds =
  map_to Sl_predsym.MSet.add Sl_predsym.MSet.empty (fun (_, (id, _)) -> id) inds

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

let rec unify ?(total=true) ?(tagpairs=false) 
    ?(sub_check=Sl_subst.trivial_check)
    ?(cont=Sl_unifier.trivial_continuation)
    ?(init_state=Sl_unifier.empty_state) inds inds' =
  if is_empty inds then
    if not total || is_empty inds' then cont init_state else None
  else
    let a = choose inds in
    let inds = remove a inds in
    let f a' =
      Sl_tpred.unify ~tagpairs
        ~sub_check
        ~cont:(fun state' -> unify ~total ~tagpairs ~sub_check ~cont ~init_state:state' inds (remove a' inds'))
        ~init_state a a' in
    find_map f inds'

let subsumed_upto_tags ?(total=true) eqs inds inds' =
  let rec aux uinds uinds' = 
    if Sl_pred.MSet.is_empty uinds then not total || Sl_pred.MSet.is_empty uinds' else
    let uind = Sl_pred.MSet.choose uinds in
    let uinds = Sl_pred.MSet.remove uind uinds in
    let uind = Sl_pred.norm eqs uind in
    match 
      Sl_pred.MSet.find_opt 
        (fun uind' -> Sl_pred.equal uind (Sl_pred.norm eqs uind')) uinds' with
    | None -> false
    | Some uind' -> aux uinds (Sl_pred.MSet.remove uind' uinds') in
  let (uinds, uinds') = Pair.map strip_tags (inds, inds') in
  aux uinds uinds'  
    
let rec subsumed ?(total=true) eqs inds inds' =
  if is_empty inds then not total || is_empty inds' else
  let ind = choose inds in
  let inds = remove ind inds in
  let ind = Sl_tpred.norm eqs ind in
  match find_opt (fun ind' -> Sl_tpred.equal ind (Sl_tpred.norm eqs ind')) inds' with
  | None -> false
  | Some ind' -> subsumed ~total eqs inds (remove ind' inds')

let norm eqs inds = endomap (Sl_tpred.norm eqs) inds