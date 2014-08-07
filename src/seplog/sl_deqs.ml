open Lib
open Util
open Symbols
open MParser

include MakeListSet(Sl_tpair)
(* if a pair contains an exist. variable then the first comp of the    *)
(* pair is an exist. var                                               *)
let add p deqs = add (Sl_tpair.norm p) deqs
let singleton p = singleton (Sl_tpair.norm p)
let mem p deqs = mem (Sl_tpair.norm p) deqs
let endomap f s = endomap (fun e -> Sl_tpair.norm (f e)) s
let subst theta m = endomap (Sl_tpair.subst theta) m

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

let subsumed eqs deqs deqs' =
  let theta = Sl_uf.to_subst eqs in
  subset (subst theta deqs) (subst theta deqs')

let part_unify cont theta d d' =
  Sl_tpair.FList.part_unify cont theta (to_list d) (to_list d')
