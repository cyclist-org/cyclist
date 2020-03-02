open Lib
open   Symbols
open Generic

open MParser

include Prod.Set

let dest f =
  if not (Int.equal (cardinal f) 1)
    then raise Prod.Not_product
    else min_elt f
let tags f = Tags.union_of_list (Blist.map Prod.tags (elements f))
let to_string d =
  if is_empty d then "F" else
    Blist.to_string " \\/ " Prod.to_string (elements d)

let pp_disj fmt () =
  Format.pp_print_string fmt " \\/" ; Format.pp_print_space fmt ()
let pp fmt f =
  if is_empty f then
    Format.fprintf fmt "@[F@]"
  else
    Format.fprintf fmt "@[%a@]" (Blist.pp pp_disj Prod.pp) (elements f)

let parse st =
  (sep_by1 Prod.parse (parse_symb symb_or) |>> of_list <?> "Form") st

let terms f = map_to Term.Set.union Term.Set.empty Prod.terms f
let tag_pairs f = Tagpairs.mk (tags f)

(* f2 |- f1 *)
let subsumed_wrt_tags t f1 f2 =
  for_all (fun d2 ->
    exists (fun d1 ->
      Prod.subsumed_wrt_tags t d1 d2) f1) f2

let equal_upto_tags f f' =
  let subsumed f1 f2 =
    for_all
      (fun a -> exists (fun a' -> Prod.equal_upto_tags a a') f2) f1 in
  (subsumed f f') && (subsumed f' f)

let rec uni_subsumption left fhook theta f f' =
  if is_empty f' then fhook theta else
  let p' = choose f' in
  let f' = remove p' f' in
  let hook' theta' =
    uni_subsumption left fhook theta' f f' in
  let g p = Prod.uni_subsumption left hook' theta p p' in
  Blist.find_map g (elements f)

let left_subsumption fhook theta f f' =
  uni_subsumption true fhook theta f f'
let right_subsumption fhook theta f f' =
  uni_subsumption false fhook theta f f'

let subst theta f = map (Prod.subst theta) f

let is_prod f = Int.equal (cardinal f) 1
