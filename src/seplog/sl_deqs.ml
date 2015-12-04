open Lib
open Util
open Symbols
open MParser

include Sl_tpair.ListSet

let add p deqs = add (Sl_tpair.order p) deqs
let singleton p = singleton (Sl_tpair.order p)
let mem p deqs = mem (Sl_tpair.order p) deqs
let endomap f s = endomap (fun e -> Sl_tpair.order (f e)) s
let subst theta m = endomap (Sl_tpair.subst theta) m
let of_list l = Blist.foldl (fun deqs p -> add p deqs) empty l

let to_string_list v = Blist.map (Sl_tpair.to_string_sep symb_deq.str) (elements v)
let to_string v =
  Blist.to_string symb_star.sep (Sl_tpair.to_string_sep symb_deq.str) (elements v)
let to_melt v =
  ltx_star (Blist.map (Sl_tpair.to_melt_sep symb_deq.melt) (elements v))

let terms d = Sl_tpair.FList.terms (elements d)

let vars d = Sl_term.filter_vars (terms d)

let parse st =
  (Sl_term.parse >>= (fun x ->
          parse_symb symb_deq >>
          Sl_term.parse << spaces |>> (fun y -> (x, y))) <?> "deq") st

let unify_partial ?(inverse=false) ?(update_check=Fun._true)
    d d' cont init_state =
  mk_unifier false false (Fun.direct inverse (Sl_tpair.unify ~update_check)) 
    d d' cont init_state
    
let biunify_partial ?(update_check=Fun._true) d d' cont init_state =
  mk_unifier false false (Sl_tpair.biunify ~update_check) d d' cont init_state

let subsumed eqs deqs deqs' =
  let norm p = Sl_tpair.order (Pair.map (fun x -> Sl_uf.find x eqs) p) in
  for_all
    (fun p ->
      let p' = norm p in 
      exists (fun q -> Sl_tpair.equal p' (norm q)) deqs'
      ) 
    deqs

let norm eqs deqs =
  endomap 
    (fun p -> Sl_tpair.order (Pair.map (fun x -> Sl_uf.find x eqs) p)) 
    deqs
   