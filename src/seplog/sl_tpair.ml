open Lib

open Symbols
open MParser

module TPair = Pair.Make(Sl_term)(Sl_term)

include TPair

let _unify update_check (x, y) (x', y') cont init_state =
  Sl_unify.Unidirectional.unify_trm ~update_check x x'
  (Sl_unify.Unidirectional.unify_trm ~update_check y y' cont)
  init_state

let _biunify update_check (x, y) (x', y') cont init_state =
  Sl_unify.Bidirectional.unify_trm ~update_check x x'
  (Sl_unify.Bidirectional.unify_trm ~update_check y y' cont)
  init_state

let mk_unify unify order p p' cont init_state =
  if order then
    unify p p' cont init_state
  else
    Blist.find_map (fun p' -> unify p p' cont init_state) [ p'; Pair.swap p' ]

let unify ?(order=false) ?(update_check=Fun._true)
    p p' cont init_state =
  mk_unify (_unify update_check) order p p' cont init_state

let biunify ?(order=false) ?(update_check=Fun._true)
    p p' cont init_state =
  mk_unify (_biunify update_check) order p p' cont init_state

let order ((x,y) as pair) =
  if Int.(<=) (Sl_term.compare x y) 0 then pair else (y,x)

let subst theta (x,y) = (Sl_subst.apply theta x, Sl_subst.apply theta y)

let to_string_sep sep p =
  let (x,y) = Pair.map Sl_term.to_string p in x ^ sep ^ y

module FList =
  struct
    include Flist.Make(TPair)

    (* let rec unify_partial ?(order=false) ?(inverse=false)                                                  *)
    (*     ?(sub_check=Sl_subst.trivial_check)                                                                *)
    (*     ?(cont=Sl_unifier.trivial_continuation)                                                            *)
    (*     ?(init_state=Sl_unifier.empty_state) xs ys =                                                       *)
    (*   match (xs, ys) with                                                                                  *)
    (*   | ([], _) -> cont init_state                                                                         *)
    (*   | (_, []) -> None                                                                                    *)
    (*   | (p::ps, _) ->                                                                                      *)
    (*     Blist.find_map                                                                                     *)
    (*       (fun q ->                                                                                        *)
    (*         let (x,y) = if inverse then (q,p) else (p,q) in                                                *)
    (*         unify ~order ~sub_check                                                                        *)
    (*           ~cont:(fun state' ->                                                                         *)
    (*             unify_partial ~order ~inverse ~sub_check ~cont ~init_state:state' ps ys) ~init_state x y)  *)
    (*       ys                                                                                               *)

    let terms ps =
      Blist.foldl
        (fun a p -> Pair.fold Sl_term.Set.add p a)
        Sl_term.Set.empty
        ps
  end

module ListSet = Listset.Make(TPair)
