open Lib
open   Symbols

open Generic

open MParser

include Pair.Make (Ord_constraints) (Flist.Make (Pheap))

let empty = (Ord_constraints.empty, [Pheap.empty])

exception Not_symheap

let is_symheap = function _, [s] -> true | _ -> false

let dest : t -> Ord_constraints.t * Pheap.t = function
  | cs, [s] -> (cs, s)
  | _ -> raise Not_symheap

(* Another way to write the same:
let dest p = match p with
  | cs, [s] -> cs, s
  | _ -> raise Not_symheap *)

(* dest function for pheaps: *)
let dest_csl (ph: Pheap.t) = 
  dest (Ord_constraints.empty, [ph]) 
 

let constraints_sep cs =
  if Ord_constraints.is_empty cs then symb_nullstr else symb_colon

let pp fmt (cs, f) =
  let pp_or fmt () = Format.fprintf fmt " %s@ " symb_or.str in
  match f with
  | [] ->
      Format.fprintf fmt "@[%a%s%s@]" Ord_constraints.pp cs
        (constraints_sep cs).sep symb_false.str
  | _ ->
      Format.fprintf fmt "@[%a%s%a@]" Ord_constraints.pp cs
        (constraints_sep cs).sep
        (Blist.pp pp_or Pheap.pp)
        f

let to_string (cs, hs) =
  let cs_str = Ord_constraints.to_string cs ^ (constraints_sep cs).sep in
  match hs with
  | [] -> cs_str ^ symb_false.str
  | hs -> cs_str ^ Blist.to_string symb_or.sep Pheap.to_string hs

let terms (_, d) = Term.Set.union_of_list (Blist.map Pheap.terms d)

let vars f = Term.filter_vars (terms f)

let tags (cs, d) =
  Tags.union_of_list (Ord_constraints.tags cs :: Blist.map Pheap.tags d)

let tag_pairs f = Tagpairs.mk (tags f)

let inconsistent (cs, f) =
  Ord_constraints.inconsistent cs || Blist.for_all Pheap.inconsistent f

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
             if Pheap.has_untagged_preds h then
               let avoid' =
                 Blist.foldl
                   (fun ts h -> Tags.union ts (Pheap.tags h))
                   avoid hs'
               in
               Pheap.complete_tags avoid' h
             else h
           in
           h' :: hs' )
         (Blist.rev hs) [])
  in
  (cs, hs)

let subsumed_upto_constraints ?(total = true) (_, hs) (_, hs') =
  Blist.for_all
    (fun d2 -> Blist.exists (fun d1 -> Pheap.subsumed ~total d1 d2) hs)
    hs'

let subsumed ?(total = true) ((cs, _) as l) ((cs', _) as r) =
  subsumed_upto_constraints ~total l r
  &&
  let () =
    debug (fun _ ->
        "Checking constraint subsumption: "
        ^ Ord_constraints.to_string cs'
        ^ " |- "
        ^ Ord_constraints.to_string cs )
  in
  let cs' = Ord_constraints.close cs' in
  Ord_constraints.subsumes cs' cs

let subsumed_upto_tags ?(total = true) (cs, hs) (cs', hs') =
  Blist.for_all
    (fun d2 ->
      Blist.exists (fun d1 -> Pheap.subsumed_upto_tags ~total d1 d2) hs )
    hs'

let equal_upto_tags (cs, hs) (cs', hs') =
  Blist.for_all2 Pheap.equal_upto_tags hs hs'

let parse ?(null_is_emp = false) ?(allow_tags = true) ?(augment_deqs = true) st
    =
  ( (if allow_tags then option Ord_constraints.parse else return None)
  >>= fun cs ->
  sep_by (Pheap.parse ~allow_tags ~augment_deqs) (parse_symb symb_or)
  <?> "formula"
  >>= fun hs ->
  return
    ( Option.dest Ord_constraints.empty Fun.id cs
    , if null_is_emp && Blist.is_empty hs then [Pheap.empty] else hs ) )
    st

let of_string ?(null_is_emp = false) s =
  handle_reply (MParser.parse_string (parse ~null_is_emp) s ())

let star ?(augment_deqs = true) (cs, f) (cs', g) =
  let hs =
    Blist.map
      (fun (f', g') -> Pheap.star ~augment_deqs f' g')
      (Blist.cartesian_product f g)
  in
  let constraints = Ord_constraints.union cs cs' in
  (constraints, hs)

let disj (cs, f) (cs', g) = (Ord_constraints.union cs cs', f @ g)

let subst theta (cs, hs) = (cs, Blist.map (fun h -> Pheap.subst theta h) hs)

let subst_existentials (cs, hs) = (cs, Blist.map Pheap.subst_existentials hs)

let subst_tags tagpairs (cs, hs) =
  ( Ord_constraints.subst_tags tagpairs cs
  , Blist.map (Pheap.subst_tags tagpairs) hs )

let norm (cs, hs) = (cs, Blist.map Pheap.norm hs)

let with_constraints (_, hs) cs = (cs, hs)

let with_heaps (cs, _) hs = (cs, hs)

let add_constraints (cs, hs) cs' = (Ord_constraints.union cs cs', hs)

let get_tracepairs f ((cs, _) as f') =
  let cs = Ord_constraints.close cs in
  let id_pairs = Tagpairs.mk (Pair.apply Tags.inter (Pair.map tags (f, f'))) in
  let allpairs, progressing =
    Pair.map
      (fun tps ->
        Tagpairs.map Pair.swap
          (Tagpairs.filter (fun (_, t) -> Tags.mem t (tags f)) tps) )
      ( Tagpairs.union id_pairs (Ord_constraints.all_pairs cs)
      , Ord_constraints.prog_pairs cs )
  in
  (allpairs, progressing)

let compute_frame ?(freshen_existentials = true)
    ?(avoid = (Tags.empty, Term.Set.empty)) f f' =
  try
    let cs, h = dest f in
    let cs', h' = dest f' in
    let eqs, deqs, ptos, inds = Pheap.dest h.heap in
    let eqs', deqs', ptos', inds' = Pheap.dest h'.heap in
    if not (Ord_constraints.subset cs cs') then None
    else if not (Uf.subsumed eqs eqs') then None
    else if not (Deqs.subset deqs deqs') then None
    else if not (Ptos.subset ptos ptos') then None
    else if not (Tpreds.subset inds inds') then None
    else
      let ex_tags = Tags.filter Tags.is_exist_var (Pheap.tags h.heap) in
      let ex_vars =
        Term.Set.filter Term.is_exist_var (Pheap.terms h.heap)
      in
      let frame =
        ( Ord_constraints.diff cs' cs
        , [ Pheap.mk_h (Uf.diff eqs eqs') (Deqs.diff deqs' deqs)
              (Ptos.diff ptos' ptos)
              (Tpreds.diff inds' inds) ] )
      in
        let ex_frame_tags = Tags.filter Tags.is_exist_var (tags frame) in
      let ex_frame_vars =
        Term.Set.filter Term.is_exist_var (terms frame)
      in
      let clashing_tags = Tags.inter ex_tags ex_frame_tags in
      let clashing_vars = Term.Set.inter ex_vars ex_frame_vars in
      if
        (not freshen_existentials)
        && ( (not (Tags.is_empty clashing_tags))
           || not (Term.Set.is_empty clashing_vars) )
      then None
      else
        let tag_subst =
          Tagpairs.mk_ex_subst (Tags.union (fst avoid) (tags f')) clashing_tags
        in
        let trm_subst =
          Subst.mk_ex_subst
            (Term.Set.union (snd avoid) (terms f'))
            clashing_vars
        in
        Some (subst trm_subst (subst_tags tag_subst frame))
  with Not_symheap -> None
