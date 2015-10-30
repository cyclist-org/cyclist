open Lib
open Util
open Symbols
open MParser

include PairTypes(Ord_constraints)(MakeFList(Sl_heap))

let empty = (Ord_constraints.empty, [ Sl_heap.empty ])

exception Not_symheap
let is_symheap = function
  | (_, [s]) -> true
  | _ -> false
let dest = function
  | (cs, [s]) -> (cs, s)
  | _ -> raise Not_symheap

let constraints_sep cs = if (Ord_constraints.is_empty cs) then symb_nullstr else symb_colon

let pp fmt (cs, f) =
  let pp_or fmt () = Format.fprintf fmt " %s@ " symb_or.str in
  if f <> [] then
    Format.fprintf fmt "@[%a%s%a@]" Ord_constraints.pp cs (constraints_sep cs).sep (Blist.pp pp_or Sl_heap.pp) f
  else
    Format.fprintf fmt "@[%a%s%s@]" Ord_constraints.pp cs (constraints_sep cs).sep symb_false.str

let to_string (cs, hs) =
  let cs_str = (Ord_constraints.to_string cs) ^ (constraints_sep cs).sep in
  match hs with
  | [] -> cs_str ^ symb_false.str
  | hs ->  cs_str ^ (Blist.to_string symb_or.sep Sl_heap.to_string hs)

let to_melt (cs, hs) =
  ltx_mk_math
    (Latex.concat
      [ (Ord_constraints.to_melt cs) ;
        (constraints_sep cs).melt ;
        (if hs = [] then symb_false.melt else
          Latex.concat
            (Latex.list_insert symb_or.melt (Blist.map Sl_heap.to_melt hs)))])

let terms (_, d) = Sl_term.Set.union_of_list (Blist.map Sl_heap.terms d)
let vars f = Sl_term.filter_vars (terms f)
let tags (cs, d) = Tags.union_of_list ((Ord_constraints.tags cs)::(Blist.map Sl_heap.tags d))
let tag_pairs f = TagPairs.mk (tags f)
let inconsistent (cs, f) = (Ord_constraints.inconsistent cs) || (Blist.for_all Sl_heap.inconsistent f)

let complete_tags avoid (cs, hs) =
  let hs = 
    Blist.rev 
      (Blist.foldr 
        (fun h hs' -> 
          let h' =
            (* This conditional is an attempt at making efficiency savings:       *)
            (* if we will not be generating new tags for this particular disjunct *)
            (* then don't bother calculating the avoid set - a computation which  *)
            (* involves progressively more duplicated work as the fold progresses *) 
            if Sl_heap.has_untagged_preds h then
              let avoid' = Blist.foldl (fun ts h -> Tags.union ts (Sl_heap.tags h)) avoid hs' in
              Sl_heap.complete_tags avoid' h
            else h in
          h'::hs') 
        (Blist.rev hs) 
        []) in
  (cs, hs)

let subsumed_upto_constraints ?(total=true) (_, hs) (_, hs') =
  Blist.for_all (fun d2 ->
    Blist.exists (fun d1 ->
      Sl_heap.subsumed ~total d1 d2) hs) hs'

let subsumed ?(total=true) ((cs, _) as l) ((cs', _) as r) =
  subsumed_upto_constraints ~total l r  && 
  let () = debug (fun _ -> "Checking constraint subsumption: " ^ (Ord_constraints.to_string cs') ^ " |- " ^ (Ord_constraints.to_string cs)) in
  let cs' = Ord_constraints.close cs' in
  Ord_constraints.subsumed cs' cs
      
let subsumed_upto_tags ?(total=true) (cs, hs) (cs', hs') =
  Blist.for_all (fun d2 ->
    Blist.exists (fun d1 ->
      Sl_heap.subsumed_upto_tags ~total d1 d2) hs) hs'

let equal_upto_tags (cs, hs) (cs', hs') =
  Blist.for_all2 Sl_heap.equal_upto_tags hs hs'


let parse st =
  (option Ord_constraints.parse >>= (fun cs ->
  (sep_by1 Sl_heap.parse (parse_symb symb_or) <?> "formula") >>= (fun hs ->
  return (Option.dest Ord_constraints.empty Fun.id cs, hs)))) st
let of_string s =
  handle_reply (MParser.parse_string parse s ())

let star (cs, f) (cs', g) =
  let hs = Blist.map (fun (f', g') -> Sl_heap.star f' g') (Blist.cartesian_product f g) in
  let constraints = Ord_constraints.union cs cs' in
  (constraints, hs)

let disj (cs, f) (cs', g) = ((Ord_constraints.union cs cs'), f @ g)

let subst theta (cs, hs) = (cs, Blist.map (fun h -> Sl_heap.subst theta h) hs)
let subst_existentials (cs, hs) = (cs, Blist.map Sl_heap.subst_existentials hs)
let subst_tags tagpairs (cs, hs) =
  (Ord_constraints.subst_tags tagpairs cs,
    Blist.map (Sl_heap.subst_tags tagpairs) hs)
let norm (cs, hs) = (cs, Blist.map Sl_heap.norm hs)

let with_constraints (_, hs) cs = (cs, hs)
let with_heaps (cs, _) hs = (cs, hs)
