(* include Treeset.Make(Pair.Make(Tags.Elt)(Tags.Elt)) *)
open Lib

include Tags.Elt.Unifier

let mk s = Tags.fold (fun i p -> add (i, i) p) s empty

let compose t1 t2 =
  fold
    (fun (x, y) a ->
      fold
        (fun (w, z) b -> if Tags.Elt.equal y w then add (x, z) b else b)
        t2 a )
    t1 empty

let projectl tp = map_to Tags.add Tags.empty fst tp

let projectr tp = map_to Tags.add Tags.empty snd tp

let reflect tps = map_to add empty Pair.swap tps

let dest_singleton tps =
  Option.mk_lazily (cardinal tps == 1) (fun _ -> choose tps)

let apply_to_tag tps t =
  try snd (find (fun (t', _) -> Tags.Elt.equal t t') tps) with Not_found -> t

let strip tps = filter (fun (t, t') -> not (Tags.Elt.equal t t')) tps

let flatten tps =
  fold (fun (t, t') ts -> Tags.add t' (Tags.add t ts)) tps Tags.empty

let mk_subst avoid ts fresh_vars =
  let ts' = fresh_vars (Tags.union ts avoid) (Tags.cardinal ts) in
  of_list (Blist.combine (Tags.to_list ts) ts')

let mk_free_subst avoid ts = mk_subst avoid ts Tags.fresh_fvars

let mk_ex_subst avoid ts = mk_subst avoid ts Tags.fresh_evars

let partition_subst theta =
  partition (fun tp -> Pair.both (Pair.map Tags.is_free_var tp)) theta
