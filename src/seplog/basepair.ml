open Lib
open   Symbols

open Generic

open MParser

module SH = Heap
module AllocatedT = Pair.Make (Term) (Int)

module Allocated = struct
  include Treeset.Make (AllocatedT)

  let terms s = map_to Term.Set.add Term.Set.empty fst s

  let vars s = Term.filter_vars (terms s)

  let endomap_fst f s = map (fun (x, i) -> (f x, i)) s
end

module BasePair = struct
  module T = Pair.Make (Allocated) (Heap)
  include T
  include Containers.Make (T)

  let to_string (v, g) =
    symb_lp.str ^ symb_lb.str
    ^ Blist.to_string symb_comma.sep AllocatedT.to_string (Allocated.to_list v)
    ^ symb_rb.str ^ symb_comma.sep ^ Heap.to_string g ^ symb_rp.str

  let pp fmt (v, g) =
    Format.fprintf fmt "@[%s%s%a%s%s%a%s@]" symb_lp.str symb_lb.str
      (Blist.pp pp_commasp AllocatedT.pp)
      (Allocated.to_list v) symb_rb.str symb_comma.sep Heap.pp g symb_rp.str

  let vars (v, h) = Term.Set.union (Allocated.vars v) (Heap.vars h)
 
  let norm (v, h) =
    let h' = Heap.norm h in
    let v' = Allocated.endomap_fst (fun x -> Uf.find x h'.Heap.eqs) v in
    (v', h')

  (* pre: g is consistent *)
  let project (v, g) case =
    let formals = Indrule.formals case in
    let formals_set = Term.Set.of_list formals in
    let g' = Heap.project g formals in
    let v' =
      Allocated.filter (fun (x, _) -> Term.Set.mem x formals_set) v
    in
    norm (v', g')

  let subst theta (v, g) =
    let v' = Allocated.endomap_fst (fun z -> Subst.apply theta z) v in
    let g' = Heap.subst theta g in
    (v', g')

  let unfold (v, h) (_, (_, params)) ((v', g'), formals) =
    (* simultaneously freshen case and (v',g') *)
    let avoidvars = Term.Set.union (Allocated.vars v) (Heap.vars h) in
    let theta = Subst.avoid avoidvars (Term.Set.of_list formals) in
    (* let theta = Subst.avoid avoidvars (Indrule.vars case) in *)
    let formals = Blist.map (fun x -> Subst.apply theta x) formals in
    let v', g' = subst theta (v', g') in
    (* now carry on with substitution as normal *)
    (* let formals = Indrule.formals case in *)
    let theta = Term.Map.of_list (Blist.combine formals params) in
    let v', g' = subst theta (v', g') in
    let h' = Heap.star h g' in
    let deqs =
      Allocated.fold
        (fun (x, _) deqs ->
          Allocated.fold (fun (y, _) deqs' -> Deqs.add (x, y) deqs') v' deqs
          )
        v h'.SH.deqs
    in
    let h' = SH.with_deqs h' deqs in  
    let v = Allocated.union v v' in
    (v, h')

  (* assumes case is built with Heap.star so ys are already unequal *)
  let unfold_all case cbps =
    let h, _ = Indrule.dest case in
    (* let () = assert (Tpreds.cardinal h.inds = Blist.length cbps) in *)
    let ys =
      Ptos.map_to Allocated.add Allocated.empty Pto.record_type h.heap.SH.ptos
    in
    let h' = SH.with_ptos h.heap Ptos.empty in
    let h' = SH.with_inds h' Tpreds.empty in
    Blist.fold_left2 unfold (ys, h') (Tpreds.to_list h.heap.SH.inds) cbps

  let gen case cbps =
    let args = Indrule.formals case in
    let v, h = unfold_all case cbps in
    if Heap.inconsistent h then None
    else
      let allvars =
        Blist.rev_append (Term.Set.to_list (Heap.vars h)) args
      in
      let v' =
        Allocated.fold
          (fun (x, i) v'' ->
            let equals = Blist.rev_filter (Heap.equates h x) allvars in
            Blist.map_to Allocated.add v'' (fun y -> (y, i)) equals )
          v Allocated.empty
      in
      Some (project (v', h) case)

  let leq (v, h) (v', h') =
    Heap.subsumed h h'
    &&
    let v, v' =
      (* use stronger heap to rewrite both variable sets *)
      Pair.map
        (Allocated.endomap_fst (fun x -> Uf.find x h'.Heap.eqs))
        (v, v')
    in
    Allocated.subset v v'
end

module BaseAndRule = struct
  module T = Pair.Make (BasePair) (Term.FList)
  include T
  include Containers.Make (T)
end

module Attempted = Hashset.Make (BaseAndRule.FList)
include BasePair
module RuleMap = Treemap.Make (Indrule)
module PredMap = Treemap.Make (Predsym)

let gen_pair case cbps s s' att =
  if Attempted.mem att cbps then ()
  else
    match gen case cbps with
    | None -> ()
    | Some bp ->
        Hashset.add s bp ;
        BaseAndRule.Hashset.add s' (bp, Indrule.formals case) ;
        Attempted.add att cbps

let choose_iter f ys =
  let rec aux f acc = function
    | [] -> f acc
    | xs :: tl -> BaseAndRule.Hashset.iter (fun x -> aux f (x :: acc) tl) xs
  in
  aux f [] (Blist.rev ys)

(* the f argument is a unit-taking function that checks if we can stop *)
(* due to first predicate being non-empty *)
let gen_pairs f case cmap pmap attmap =
  let h, (pred, _) = Indrule.dest case in
  let candidates =
    Tpreds.map_to_list (fun (_, (i, _)) -> PredMap.find i pmap) h.SH.inds
  in
  let s = RuleMap.find case cmap in
  let s' = PredMap.find pred pmap in
  let att = RuleMap.find case attmap in
  choose_iter
    (fun cbps ->
      f () ;
      gen_pair case cbps s s' att )
    candidates

let first_pred_not_empty defs =
  let defs = Defs.to_list defs in
  let first_pred = Preddef.predsym (Blist.hd defs) in
  fun pmap -> not (BaseAndRule.Hashset.is_empty (PredMap.find first_pred pmap))

exception FirstNonEmpty

let gen_all_pairs ?(only_first = false) defs =
  let first_not_empty = first_pred_not_empty defs in
  let defs = Defs.to_list defs in
  let cmap =
    Blist.fold_left
      (fun m d ->
        Blist.fold_left
          (fun m' c -> RuleMap.add c (Hashset.create 11) m')
          m (Preddef.rules d) )
      RuleMap.empty defs
  in
  let get_sizemap cmap =
    RuleMap.fold
      (fun c s m -> RuleMap.add c (Hashset.cardinal s) m)
      cmap RuleMap.empty
  in
  let pmap =
    Blist.fold_left
      (fun m d ->
        let first = Blist.hd (Preddef.rules d) in
        PredMap.add (Indrule.predsym first)
          (BaseAndRule.Hashset.create 11)
          m )
      PredMap.empty defs
  in
  let att =
    Blist.fold_left
      (fun m d ->
        Blist.fold_left
          (fun m' c -> RuleMap.add c (Attempted.create 11) m')
          m (Preddef.rules d) )
      RuleMap.empty defs
  in
  let progress = ref true in
  (* f is passed down to gen_pairs so that execution can terminate immediately *)
  (* once the first predicate is non-empty, if asked by the caller. *)
  (* this is meant to happen during iteration as opposed to after each approximant *)
  (* has been computed. *)
  (* NB we can do the below without arguments only because the key-to-value *)
  (* relationship is immutable, and as values are mutable. *)
  let f () = if only_first && first_not_empty pmap then raise FirstNonEmpty in
  let sizemap = ref (get_sizemap cmap) in
  let () =
    try
      while !progress do
        debug (fun () -> "\n" ^ RuleMap.to_string Hashset.to_string cmap ^ "\n") ;
        RuleMap.iter (fun c _ -> gen_pairs f c cmap pmap att) cmap ;
        let newsizemap = get_sizemap cmap in
        progress := not (RuleMap.equal Int.equal !sizemap newsizemap) ;
        sizemap := newsizemap
      done
    with FirstNonEmpty -> ()
  in
  (cmap, pmap)

(* NB correctness relies on rules being explicit about x->_ implying       *)
(* x!=nil !!!                                                              *)
let satisfiable ?(only_first = false) ?(output = false) defs =
  Stats.CC.call () ;
  let cmap, pmap = gen_all_pairs ~only_first defs in
  ( if output then
    let element_conv (c, s) =
      Indrule.to_string c ^ " has base " ^ Hashset.to_string s
    in
    print_endline (Blist.to_string "\n" element_conv (RuleMap.to_list cmap)) ) ;
  let retval =
    (only_first && first_pred_not_empty defs pmap)
    || (not only_first)
       && RuleMap.for_all (fun _ s -> not (Hashset.is_empty s)) cmap
  in
  if retval then Stats.CC.accept () else Stats.CC.reject () ;
  retval

let form_sat defs f = satisfiable ~only_first:true (Defs.of_formula defs f)

let pairs_of_form defs f =
  let defs = Defs.of_formula defs f in
  let bp_map, _ = gen_all_pairs defs in
  let rules, _ = Preddef.dest (Blist.hd (Defs.to_list defs)) in
  let s = Hashset.create 11 in
  let () =
    Blist.iter
      (fun r ->
        let _ = Hashset.left_union s (RuleMap.find r bp_map) in
        () )
      rules
  in
  Hashset.map_to Set.add Set.empty Fun.id s

let minimise bps =
  let res =
    Set.fold
      (fun x bps' ->
        if Set.exists (fun y -> BasePair.leq y x) bps' then bps'
        else Set.add x bps' )
      bps Set.empty
  in
  (* if not (Set.equal res bps) then print_endline "~" ;  *)
  res
