open Lib
open Util
open Symbols
open MParser

include MakeFList(Asl_heap)

let empty = [ Asl_heap.empty ]

exception Not_symheap
let is_symheap = function
  | [s] -> true
  | _ -> false
let dest = function
  | [s] -> s
  | _ -> raise Not_symheap


let inconsistent f =
  Blist.for_all Asl_sat.is_unsat (Blist.map Asl_heap.satisfiable_z3 f)

let pp fmt f =
  let pp_or fmt () = Format.fprintf fmt " %s@ " symb_or.str in
  if f <>[] then
    Format.fprintf fmt "@[%a@]" (Blist.pp pp_or Asl_heap.pp) f
  else
    Format.fprintf fmt "@[%s@]" symb_false.str

let to_string = function
  | [] -> symb_false.str
  | d ->  Blist.to_string symb_or.sep Asl_heap.to_string d

let to_melt d =
  ltx_mk_math
    (if d =[] then symb_false.melt else
        Latex.concat
          (Latex.list_insert symb_or.melt (Blist.map Asl_heap.to_melt d)))

let terms d = Asl_term.Set.union_of_list (Blist.map Asl_heap.terms d)
let vars d = Asl_term.filter_vars (terms d)

let subsumed ?(total=true) f1 f2 =
  Blist.for_all (fun d2 ->
    Blist.exists (fun d1 ->
      Asl_heap.subsumed ~total d1 d2) f1) f2
let subsumed_upto_tags ?(total=true) f1 f2 = subsumed ~total f1 f2

let equal_upto_tags f f' =
  Blist.for_all2 Asl_heap.equal_upto_tags f f'
  

let parse ?(null_is_emp=false) st =
  ((sep_by Asl_heap.parse (parse_symb symb_or) <?> "formula") >>= (fun hs ->
    return 
      (if null_is_emp && Blist.is_empty hs then [ Asl_heap.empty ] else hs))) st
let of_string ?(null_is_emp=false) s =
  handle_reply (MParser.parse_string (parse ~null_is_emp) s ())

let star f g =
  Blist.map (fun (f', g') -> Asl_heap.star f' g') (Blist.cartesian_product f g)

let disj f g = f @ g

let subst theta f = Blist.map (fun h -> Asl_heap.subst theta h) f
let subst_existentials f = Blist.map Asl_heap.subst_existentials f
let simplify_terms f = Blist.map Asl_heap.simplify_terms f
let norm f = Blist.map Asl_heap.norm f
