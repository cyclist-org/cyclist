open Lib
open Util
open Symbols
open MParser

include MakeMultiset(Sld_tpred)

let subst theta elts = endomap (Sld_tpred.subst theta) elts

let terms inds =
  Sld_term.Set.union_of_list (Blist.map Sld_tpred.terms  (elements inds))

let vars inds = Sld_term.filter_vars (terms inds)

let idents inds =
  map_to Sld_predsym.MSet.add Sld_predsym.MSet.empty (fun (_, (id, _)) -> id) inds

let to_string_list v = Blist.map Sld_tpred.to_string (elements v)
let to_string v =
  Blist.to_string symb_star.sep Sld_tpred.to_string (elements v)
let to_melt v =
  ltx_star (Blist.map Sld_tpred.to_melt (elements v))


let tags inds = Tags.of_list (Blist.map fst (to_list inds))

let strip_tags inds = 
  map_to Sld_pred.MSet.add Sld_pred.MSet.empty snd inds

let equal_upto_tags inds inds' =
  Sld_pred.MSet.equal (strip_tags inds) (strip_tags inds')
  
let subst_tags tagpairs inds = 
  endomap (Sld_tpred.subst_tag tagpairs) inds
  

let freshen_tags inds' inds =
  if is_empty inds || is_empty inds' then inds else
    let maxtag = Tags.max_elt (tags inds') in
    let mintag = fold (fun (tag, _) a -> min tag a) inds max_int in
    let delta = 1 + maxtag - mintag in
    endomap (fun (tag, head) -> (tag + delta, head)) inds

let rec unify ?(total=true) ?(tagpairs=false) 
    ?(sub_check=Sld_term.trivial_sub_check)
    ?(cont=Sld_term.trivial_continuation)
    ?(init_state=Sld_term.empty_state) inds inds' =
  if is_empty inds then
    if not total || is_empty inds' then cont init_state else None
  else
    let a = choose inds in
    let inds = remove a inds in
    let f a' =
      Sld_tpred.unify ~tagpairs
        ~sub_check
        ~cont:(fun state' -> unify ~total ~tagpairs ~sub_check ~cont ~init_state:state' inds (remove a' inds'))
        ~init_state a a' in
    find_map f inds'

let subsumed_upto_tags ?(total=true) eqs inds inds' =
  let rec aux uinds uinds' = 
    if Sld_pred.MSet.is_empty uinds then not total || Sld_pred.MSet.is_empty uinds' else
    let uind = Sld_pred.MSet.choose uinds in
    let uinds = Sld_pred.MSet.remove uind uinds in
    let uind = Sld_pred.norm eqs uind in
    match 
      Sld_pred.MSet.find_opt 
        (fun uind' -> Sld_pred.equal uind (Sld_pred.norm eqs uind')) uinds' with
    | None -> false
    | Some uind' -> aux uinds (Sld_pred.MSet.remove uind' uinds') in
  let (uinds, uinds') = Pair.map strip_tags (inds, inds') in
  aux uinds uinds'  
    
let rec subsumed ?(total=true) eqs inds inds' =
  if is_empty inds then not total || is_empty inds' else
  let ind = choose inds in
  let inds = remove ind inds in
  let ind = Sld_tpred.norm eqs ind in
  match find_opt (fun ind' -> Sld_tpred.equal ind (Sld_tpred.norm eqs ind')) inds' with
  | None -> false
  | Some ind' -> subsumed ~total eqs inds (remove ind' inds')

let norm eqs inds = endomap (Sld_tpred.norm eqs) inds
