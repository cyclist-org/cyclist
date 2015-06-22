open Lib
open Util
open Symbols
open MParser

include MakeListSet(Sld_tpair)

let add p deqs = add (Sld_tpair.order p) deqs
let singleton p = singleton (Sld_tpair.order p)
let mem p deqs = mem (Sld_tpair.order p) deqs
let endomap f s = endomap (fun e -> Sld_tpair.order (f e)) s
let subst theta m = endomap (Sld_tpair.subst theta) m
let of_list l = Blist.foldl (fun deqs p -> add p deqs) empty l

let to_string_list v = Blist.map (Sld_tpair.to_string_sep symb_deq.str) (elements v)
let to_string v =
  Blist.to_string symb_star.sep (Sld_tpair.to_string_sep symb_deq.str) (elements v)
let to_melt v =
  ltx_star (Blist.map (Sld_tpair.to_melt_sep symb_deq.melt) (elements v))

let terms d = Sld_tpair.FList.terms (elements d)

let vars d = Sld_term.filter_vars (terms d)

let parse st =
  (Sld_term.parse >>= (fun x ->
          parse_symb symb_deq >>
          Sld_term.parse << spaces |>> (fun y -> (x, y))) <?> "deq") st

let unify_partial ?(inverse=false) ?(sub_check=Sld_term.trivial_sub_check)
    ?(cont=Sld_term.trivial_continuation) 
    ?(init_state=Sld_term.empty_state) d d' =
  Sld_tpair.FList.unify_partial 
    ~inverse ~sub_check ~cont ~init_state (to_list d) (to_list d')

let subsumed eqs deqs deqs' =
  let norm p = Sld_tpair.order (Pair.map (fun x -> Sld_uf.find x eqs) p) in
  for_all
    (fun p ->
      let p' = norm p in 
      exists (fun q -> Sld_tpair.equal p' (norm q)) deqs'
      ) 
    deqs

let norm eqs deqs =
  endomap 
    (fun p -> Sld_tpair.order (Pair.map (fun x -> Sld_uf.find x eqs) p)) 
    deqs
   
