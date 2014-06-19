open Lib
open Util
open Symheap
open Symbols
open MParser

include MakeFList(Heap)

let empty = [ Heap.empty ]

let dest (f: t) = match f with
  | [s] -> s
  | _ -> raise Not_symheap

let star f g =
  Blist.map (fun (f', g') -> Heap.star f' g') (Blist.cartesian_product f g)

let disj f g : t = f @ g
let terms d = Term.Set.union_of_list (Blist.map Heap.terms d)
let vars d = Term.filter_vars (terms d)
let to_string d = Blist.to_string symb_or.sep Heap.to_string d
let to_melt d =
  ltx_mk_math
    (if d =[] then symb_false.melt else
        Latex.concat
          (Latex.list_insert symb_or.melt (Blist.map Heap.to_melt d)))

let norm l = Blist.map Heap.norm l
let tags d = Tags.union_of_list (Blist.map Heap.tags d)
let tag_pairs f = TagPairs.mk (tags f)
let equates f x y = Blist.for_all (fun h -> Heap.equates h x y) f
let inconsistent f = Blist.for_all Heap.inconsistent f

let pp fmt f =
  let pp_or fmt () =
    Format.fprintf fmt " %s@ " symb_or.str in
  if f <>[] then
    Format.fprintf fmt "@[%a@]" (Blist.pp pp_or Heap.pp) f
  else
    Format.fprintf fmt "@[F@]"

let subst theta f = Blist.map (fun h -> Heap.subst theta h) f

let aux_subsumed_wrt_tags spw t f1 f2 =
  Blist.for_all (fun d2 ->
          Blist.exists (fun d1 ->
                  Heap.aux_subsumed_wrt_tags spw t d1 d2) f1) f2

(* f2 |- f1 *)
let subsumed_wrt_tags t f1 f2 =
  aux_subsumed_wrt_tags false t f1 f2

(* f2 |- f1 * true *)
let spw_subsumed_wrt_tags t f1 f2 =
  aux_subsumed_wrt_tags true t f1 f2

let rec aux_subsumption left spw fhook theta f f' =
  if []= f' then fhook theta else
    let p' = Blist.hd f' in
    let f' = Blist.tl f' in
    let hook' theta' =
      aux_subsumption left spw fhook theta' f f' in
    let g p = Heap.aux_subsumption left spw hook' theta p p' in
    Blist.find_some g f

let uni_subsumption left fhook theta f f' =
  aux_subsumption left false fhook theta f f'

let rec spw_left_subsumption fhook theta f f' =
  aux_subsumption true true fhook theta f f'

let left_subsumption fhook theta f f' =
  uni_subsumption true fhook theta f f'
let right_subsumption fhook theta f f' =
  uni_subsumption false fhook theta f f'

let subst_existentials f = Blist.map Heap.subst_existentials f

let is_fresh_in x f = Blist.for_all (Heap.is_fresh_in x) f
let is_heap f = Blist.length f = 1

let parse st =
  (sep_by1 Heap.parse (parse_symb symb_or) <?> "formula") st


