open Lib
open Util
open Symbols
open MParser

include MakeListSet(Sl_tpair)

let add p deqs = add (Sl_tpair.order p) deqs
let singleton p = singleton (Sl_tpair.order p)
let mem p deqs = mem (Sl_tpair.order p) deqs
let endomap f s = endomap (fun e -> Sl_tpair.order (f e)) s
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

let unify_within cont state d d' =
  Sl_tpair.FList.unord_unify_within cont state (to_list d) (to_list d')

let inverse_unify_within cont state d d' =
  Sl_tpair.FList.inverse_unord_unify_within cont state (to_list d) (to_list d')

let subsumed eqs deqs deqs' =
  (* Option.is_some                                                                   *)
  (*   (unify_within (Sl_uf.subst_subsumed eqs) (Sl_term.empty_subst, ()) deqs deqs') *)
  match
    unify_within
      (Sl_uf.subst_subsumed eqs)
      (Sl_term.empty_subst, ()) deqs deqs' with
  | None -> false
  | Some (theta, _) ->
    assert (subset (subst theta deqs) (subst theta deqs')) ; true

