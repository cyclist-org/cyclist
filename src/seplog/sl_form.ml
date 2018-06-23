open Lib

open Symbols
open MParser

include Pair.Make(Ord_constraints)(Flist.Make(Sl_heap))

let empty = (Ord_constraints.empty, [ Sl_heap.empty ])

exception Not_symheap
let is_symheap = function
  | (_, [s]) -> true
  | _ -> false
let dest : t -> Ord_constraints.t * Sl_heap.t = function
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

let terms (_, d) = Sl_term.Set.union_of_list (Blist.map Sl_heap.terms d)
let vars f = Sl_term.filter_vars (terms f)
let tags (cs, d) = Tags.union_of_list ((Ord_constraints.tags cs)::(Blist.map Sl_heap.tags d))
let tag_pairs f = Tagpairs.mk (tags f)
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
  Ord_constraints.subsumes cs' cs

let subsumed_upto_tags ?(total=true) (cs, hs) (cs', hs') =
  Blist.for_all (fun d2 ->
    Blist.exists (fun d1 ->
      Sl_heap.subsumed_upto_tags ~total d1 d2) hs) hs'

let equal_upto_tags (cs, hs) (cs', hs') =
  Blist.for_all2 Sl_heap.equal_upto_tags hs hs'


let parse ?(null_is_emp=false) ?(allow_tags=true) ?(augment_deqs=true) st =
  ((if allow_tags then (option Ord_constraints.parse) else return None) >>= (fun cs ->
  (sep_by (Sl_heap.parse ~allow_tags ~augment_deqs) (parse_symb symb_or) <?> "formula") >>= (fun hs ->
  return
    (Option.dest Ord_constraints.empty Fun.id cs,
      if null_is_emp && Blist.is_empty hs then [ Sl_heap.empty ] else hs)))) st

let of_string ?(null_is_emp=false) s =
  handle_reply (MParser.parse_string (parse ~null_is_emp) s ())

let star ?(augment_deqs=true) (cs, f) (cs', g) =
  let hs =
    Blist.map
      (fun (f', g') -> Sl_heap.star ~augment_deqs f' g')
      (Blist.cartesian_product f g) in
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

let add_constraints (cs, hs) cs' = (Ord_constraints.union cs cs', hs)

let get_tracepairs f ((cs, _) as f') =
  let cs = Ord_constraints.close cs in
  let id_pairs =
    (Tagpairs.mk (Pair.apply Tags.inter (Pair.map tags (f, f')))) in
  let (allpairs, progressing) =
    Pair.map
      (fun tps -> Tagpairs.endomap Pair.swap
        (Tagpairs.filter (fun (_, t) -> Tags.mem t (tags f)) tps))
      ((Tagpairs.union id_pairs (Ord_constraints.all_pairs cs)),
        Ord_constraints.prog_pairs cs) in
  (allpairs, progressing)

let compute_frame
    ?(freshen_existentials=true) ?(avoid=(Tags.empty, Sl_term.Set.empty)) f f' =
  try
    let (cs, h) = dest f in
    let (cs', h') = dest f' in
    let (eqs, deqs, ptos, inds) = Sl_heap.dest h in
    let (eqs', deqs', ptos', inds') = Sl_heap.dest h' in
    if
      not (Ord_constraints.subset cs cs') then None
    else if
      not (Sl_uf.subsumed eqs eqs') then None
    else if
      not (Sl_deqs.subset deqs deqs') then None
    else if
      not (Sl_ptos.subset ptos ptos') then None
    else if
      not (Sl_tpreds.subset inds inds') then None
    else
      let ex_tags = Tags.filter Tags.is_exist_var (Sl_heap.tags h) in
      let ex_vars = Sl_term.Set.filter Sl_term.is_exist_var (Sl_heap.terms h) in
      let frame =
        (Ord_constraints.diff cs' cs,
          [ Sl_heap.mk
              (Sl_uf.diff eqs eqs')
              (Sl_deqs.diff deqs' deqs)
              (Sl_ptos.diff ptos' ptos)
              (Sl_tpreds.diff inds' inds) ] ) in
      let ex_frame_tags = Tags.filter Tags.is_exist_var (tags frame) in
      let ex_frame_vars =
        Sl_term.Set.filter Sl_term.is_exist_var (terms frame) in
      let clashing_tags = Tags.inter ex_tags ex_frame_tags in
      let clashing_vars = Sl_term.Set.inter ex_vars ex_frame_vars in
      if not freshen_existentials &&
          (not (Tags.is_empty clashing_tags) ||
           not (Sl_term.Set.is_empty clashing_vars)) then None
      else
        let tag_subst =
          Tagpairs.mk_ex_subst
            (Tags.union (fst avoid) (tags f'))
            clashing_tags in
        let trm_subst =
          Sl_subst.mk_ex_subst
            (Sl_term.Set.union (snd avoid) (terms f'))
            clashing_vars in
        Some (subst trm_subst (subst_tags tag_subst frame))
  with Not_symheap -> None
