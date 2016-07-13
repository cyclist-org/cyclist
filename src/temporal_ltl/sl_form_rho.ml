open Lib
open Util
open Symbols
open MParser

include MakeFList(Sl_heap_rho)

let empty = [ Sl_heap_rho.empty ]

exception Not_symheap
let is_symheap = function
  | [s] -> true
  | _ -> false
let dest = function
  | [s] -> s
  | _ -> raise Not_symheap

let pp fmt f =
  let pp_or fmt () = Format.fprintf fmt " %s@ " symb_or.str in
  if f <>[] then
    Format.fprintf fmt "@[%a@]" (Blist.pp pp_or Sl_heap_rho.pp) f
  else
    Format.fprintf fmt "@[%s@]" symb_false.str

let to_string = function
  | [] -> symb_false.str
  | d ->  Blist.to_string symb_or.sep Sl_heap_rho.to_string d

let to_melt d =
  ltx_mk_math
    (if d =[] then symb_false.melt else
        Latex.concat
          (Latex.list_insert symb_or.melt (Blist.map Sl_heap_rho.to_melt d)))

let terms d = Sl_term.Set.union_of_list (Blist.map Sl_heap_rho.terms d)
let vars d = Sl_term.filter_vars (terms d)
let tags d = Tags.union_of_list (Blist.map Sl_heap_rho.tags d)
let tag_pairs f = TagPairs.mk (tags f)
let inconsistent f = Blist.for_all Sl_heap_rho.inconsistent f

let subsumed ?(total=true) f1 f2 =
  Blist.for_all (fun d2 ->
    Blist.exists (fun d1 ->
      Sl_heap_rho.subsumed ~total d1 d2) f1) f2
let subsumed_upto_tags ?(total=true) f1 f2 =
  Blist.for_all (fun d2 ->
    Blist.exists (fun d1 ->
      Sl_heap_rho.subsumed_upto_tags ~total d1 d2) f1) f2

let equal_upto_tags f f' =
  Blist.for_all2 Sl_heap_rho.equal_upto_tags f f'
  

let parse st =
  (sep_by1 Sl_heap_rho.parse (parse_symb symb_or) <?> "formula") st
let of_string s =
  handle_reply (MParser.parse_string parse s ())

let star f g =
  Blist.map (fun (f', g') -> Sl_heap_rho.star f' g') (Blist.cartesian_product f g)

let disj f g = f @ g

let subst theta f = Blist.map (fun h -> Sl_heap_rho.subst theta h) f
let subst_existentials f = Blist.map Sl_heap_rho.subst_existentials f
let subst_tags tagpairs f = Blist.map (Sl_heap_rho.subst_tags tagpairs) f
let norm f = Blist.map Sl_heap_rho.norm f
