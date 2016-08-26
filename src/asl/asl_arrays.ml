open Lib
open Util
open Symbols
open MParser

include MakeMultiset(Asl_array)

let subst theta arrays = endomap (Asl_array.subst theta) arrays

let to_string_list v = Blist.map Asl_array.to_string (elements v)
let to_string v =
  Blist.to_string symb_star.sep Asl_array.to_string (elements v)
let to_melt v =
  ltx_star (Blist.map Asl_array.to_melt (elements v))

let terms arrays =
  Asl_term.Set.union_of_list (Blist.map Asl_array.terms (elements arrays)) 

let vars arr = Asl_term.filter_vars (terms arr)

let rec unify ?(total=true) 
    ?(sub_check=Asl_subst.trivial_check)
    ?(cont=Asl_unifier.trivial_continuation)
    ?(init_state=Asl_unifier.empty_state) arrs arrs' =
  if is_empty arrs then
    if not total || is_empty arrs' then cont init_state else None
  else
    let a = choose arrs in
    let arrs = remove a arrs in
    let f a' =
      Asl_array.unify ~sub_check  
        ~cont:(fun state' -> 
          unify ~total ~sub_check ~cont ~init_state:state' arrs (remove a' arrs'))
        ~init_state a a' in
    find_map f arrs'

let rec subsumed ?(total=true) eqs arrs arrs' =
  if is_empty arrs then not total || is_empty arrs' else
  let arr = choose arrs in
  let arrs = remove arr arrs in
  let arr = Asl_array.norm eqs arr in
  match find_opt (fun arr' -> Asl_array.equal arr (Asl_array.norm eqs arr')) arrs' with
  | None -> false
  | Some arr' -> subsumed ~total eqs arrs (remove arr' arrs')

let norm eqs arrs = endomap (Asl_array.norm eqs) arrs
