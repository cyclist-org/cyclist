open Lib
open Symbols
open MParser
include Listset.Make (Tpair)

let add p deqs = add (Tpair.order p) deqs

let singleton p = singleton (Tpair.order p)

let mem p deqs = mem (Tpair.order p) deqs

let map f s = map (fun e -> Tpair.order (f e)) s

let subst theta m = map (Tpair.subst theta) m

let of_list l = Blist.foldl (fun deqs p -> add p deqs) empty l

let to_string_list v =
  Blist.map (Tpair.to_string_sep symb_deq.str) (elements v)

let to_string v =
  Blist.to_string symb_star.sep
    (Tpair.to_string_sep symb_deq.str)
    (elements v)

let terms d = Tpair.FList.terms (elements d)

let vars d = Term.filter_vars (terms d)

let parse st =
  ( Term.parse
  >>= (fun x ->
        parse_symb symb_deq >> Term.parse << spaces |>> fun y -> (x, y) )
  <?> "deq" )
    st

let unify_partial ?(inverse = false) ?(update_check = Fun._true) d d' cont
    init_state =
  mk_unifier false false
    (Fun.direct inverse (Tpair.unify ~update_check))
    d d' cont init_state

let biunify_partial ?(update_check = Fun._true) d d' cont init_state =
  mk_unifier false false (Tpair.biunify ~update_check) d d' cont init_state

let subsumed eqs deqs deqs' =
  let norm p = Tpair.order (Pair.map (fun x -> Uf.find x eqs) p) in
  for_all
    (fun p ->
      let p' = norm p in
      exists (fun q -> Tpair.equal p' (norm q)) deqs' )
    deqs

let norm eqs deqs =
  map (fun p -> Tpair.order (Pair.map (fun x -> Uf.find x eqs) p)) deqs
