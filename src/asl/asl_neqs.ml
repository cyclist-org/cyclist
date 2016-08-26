open Lib
open Util
open Symbols
open MParser

include MakeListSet(Asl_tpair)

let add p deqs = add (Asl_tpair.order p) deqs
let singleton p = singleton (Asl_tpair.order p)
let mem p deqs = mem (Asl_tpair.order p) deqs
let endomap f s = endomap (fun e -> Asl_tpair.order (f e)) s
let subst theta m = endomap (Asl_tpair.subst theta) m
let of_list l = Blist.foldl (fun deqs p -> add p deqs) empty l

let to_string_list v = Blist.map (Asl_tpair.to_string_sep symb_deq.str) (elements v)
let to_string v =
  Blist.to_string symb_star.sep (Asl_tpair.to_string_sep symb_deq.str) (elements v)
let to_melt v =
  ltx_star (Blist.map (Asl_tpair.to_melt_sep symb_deq.melt) (elements v))

let to_fopl p =
  let neq_to_fopl (a,b) acc = Fopl.And (Fopl.PF (Fopl.Ne (a, b)), acc)
  in fold neq_to_fopl p Fopl.trivially_true

let terms d = Blist.foldl (fun a p -> Pair.fold Asl_term.Set.add p a) Asl_term.Set.empty (elements d)

let vars d = Asl_term.filter_vars (terms d)

let parse st =
  (Asl_term.parse >>= (fun x ->
          parse_symb symb_deq >>
          Asl_term.parse << spaces |>> (fun y -> (x, y))) <?> "neq") st

let unify_partial ?(inverse=false) ?(sub_check=Asl_subst.trivial_check)
    ?(cont=Asl_unifier.trivial_continuation) 
    ?(init_state=Asl_unifier.empty_state) d d' =
  Asl_tpair.FList.unify_partial 
    ~inverse ~sub_check ~cont ~init_state (to_list d) (to_list d')

(* TODO: test wheter this is correct for asl_term(s) *)
let subsumed eqs deqs deqs' =
  let norm p = Asl_tpair.order (Pair.map (fun x -> Asl_uf.find x eqs) p) in
  for_all
    (fun p ->
      let p' = norm p in 
      exists (fun q -> Asl_tpair.equal p' (norm q)) deqs'
      ) 
    deqs

let norm eqs deqs =
  endomap 
    (fun p -> Asl_tpair.order (Pair.map (fun x -> Asl_uf.find x eqs) p)) 
    deqs
   