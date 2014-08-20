open Lib
open Util
open Symbols
open MParser

module SH = Sl_heap
  
  
module BasePair =
struct
  include PairTypes(Sl_term.Set)(Sl_heap)
  
  let to_string (v, g) =
    symb_lp.str ^
    symb_lb.str ^
    (Blist.to_string symb_comma.sep Sl_term.to_string (Sl_term.Set.to_list v)) ^
    symb_rb.str ^ symb_comma.sep ^
    (Sl_heap.to_string g) ^
    symb_rp.str
  
  let pp fmt (v, g) = 
    Format.fprintf fmt "@[%s%s%a%s%s%a%s@]"
      symb_lp.str
      symb_lb.str
      (Blist.pp pp_commasp Sl_term.pp) (Sl_term.Set.to_list v)
      symb_rb.str 
      symb_comma.sep
      Sl_heap.pp g
      symb_rp.str
  
  let project (v, g) case =
    let formals = Sl_indrule.formals case in
    (Sl_term.Set.inter v (Sl_term.Set.of_list formals), Sl_heap.project g formals)
  
  let subst theta (v, g) =
    let v' = Sl_term.Set.endomap (fun z -> Sl_term.subst theta z) v in
    let g' = Sl_heap.subst theta g in
    (v', g')
  
  let unfold (v, h) ((_, (_, params)) as ind) (case, (v', g')) =
    (* simultaneously freshen case and (v',g') *)
    let avoidvars = Sl_term.Set.union v (Sl_heap.vars h) in
    let theta = Sl_term.avoid_theta avoidvars (Sl_indrule.vars case) in
    let case = Sl_indrule.subst theta case in
    let (v', g') = subst theta (v', g') in
    (* now carry on with substitution as normal *)
    let (_, (_, formals)) = Sl_indrule.dest case in
    let theta = Sl_term.Map.of_list (Blist.combine formals params) in
    (* let formals = Sl_term.Set.of_list (Blist.map fst (Sl_term.Map.to_list     *)
    (* theta)) in let substs = Sl_term.Set.of_list (Blist.map snd             *)
    (* (Sl_term.Map.to_list theta)) in let () = require (fun () ->            *)
    (* Sl_term.Set.subset (Sl_heap.vars g') formals) in let () = assert       *)
    (* (Sl_term.Set.subset v' formals) in let () = assert (Sl_term.Set.is_empty  *)
    (* (Sl_term.Set.inter (Sl_heap.vars g') substs)) in let () = assert (     *)
    (* Sl_term.Set.is_empty (Sl_term.Set.inter v' substs)) in                    *)
    let (v', g') = subst theta (v', g') in
    let h' = SH.del_ind h ind in
    let h' = Sl_heap.star h' g' in
    let cv = Blist.cartesian_product (Sl_term.Set.to_list v) (Sl_term.Set.to_list v') in
    let h' = SH.with_deqs h' (Sl_deqs.union h'.SH.deqs (Sl_deqs.of_list cv)) in
    let v = Sl_term.Set.union v v' in
    (v, h')
  
  (* assumes case is built with Sl_heap.star so ys are already unequal *)
  let unfold_all case cbps =
    let (h, _) = Sl_indrule.dest case in
    (* let () = assert (Sl_tpreds.cardinal h.inds = Blist.length cbps) in *)
    let ys = Sl_term.Set.of_list (Blist.rev_map fst (Sl_ptos.to_list h.SH.ptos)) in
    let h = SH.with_ptos h Sl_ptos.empty in
    Blist.fold_left2 unfold (ys, h) (Sl_tpreds.to_list h.SH.inds) cbps
  
  let gen case cbps =
    let (_, (_, args)) = Sl_indrule.dest case in
    let (v, h) = unfold_all case cbps in
    if Sl_heap.inconsistent h then None else
      let l = Blist.rev_append (Sl_term.Set.to_list (Sl_heap.vars h)) args in
      let l = Blist.rev_filter
          (fun u -> Sl_term.Set.exists (fun z -> Sl_heap.equates h u z) v) l in
      let v = Sl_term.Set.of_list l in
      Some (project (v, h) case)
  
end

module BasePairSet = MakeTreeSet(BasePair)

module CaseMap =
struct
  include MakeMap(Sl_indrule)
  let to_string cmap =
    let aux (c, s) =
      (Sl_indrule.to_string c) ^ "\nBase pairs: " ^
      (BasePairSet.to_string s) ^ "\n" in
    Blist.to_string "\n" aux (to_list cmap)
end

let get_bps cmap (_, (ident, _)) =
  let l =
    Blist.filter
      (fun (c, _) -> Strng.equal ident (fst (snd (Sl_indrule.dest c))))
      (CaseMap.to_list cmap) in
  Blist.bind
    (fun (c, s) -> Blist.map (fun bp -> (c, bp)) (BasePairSet.to_list s))
    l

let gen_pairs case cmap =
  let (h, _) = Sl_indrule.dest case in
  let candidates =
    Blist.map (fun i -> get_bps cmap i) (Sl_tpreds.to_list h.SH.inds) in
  let l = Blist.choose candidates in
  let poss_bps =
    Blist.rev_map (fun cbps -> BasePair.gen case cbps) l in
  let bps = Option.list_get poss_bps in
  BasePairSet.of_list bps

let first_pred_not_empty defs =
  let first_pred = Sl_preddef.predsym (Blist.hd defs) in
  fun cm ->
      CaseMap.exists
        (fun k v ->
              Strng.equal (Sl_indrule.predsym k) first_pred &&
              not (BasePairSet.is_empty v)
        )
        cm

let gen_all_pairs defs only_first =
  let first_not_empty = first_pred_not_empty defs in
  let cmap =
    Blist.fold_left
      (fun m d ->
            Blist.fold_left
              (fun m' c -> CaseMap.add c BasePairSet.empty m') m (Sl_preddef.rules d))
      CaseMap.empty
      defs
  in
  let onestep cmap =
    let r = CaseMap.endomap
        (fun (c, s) -> (c, BasePairSet.union s (gen_pairs c cmap)))
        cmap in
    let () = debug (fun () -> "\n" ^ (CaseMap.to_string r) ^ "\n") in
    r in
  let rec fixp cm =
    let cm' = onestep cm in
    if
    only_first && first_not_empty cm' ||
    CaseMap.equal BasePairSet.equal cm cm'
    then
      cm'
    else
      fixp cm' in
  fixp cmap

(* NB correctness relies on rules being explicit about x->_ implying       *)
(* x!=nil !!!                                                              *)
let satisfiable defs only_first output =
  let defs = Sl_defs.to_list defs in
  Stats.CC.call () ;
  let res = gen_all_pairs defs only_first in
  if output then
    begin
      let element_conv (c, s) =
        ((Sl_indrule.to_string c) ^ " has base " ^ (BasePairSet.to_string s)) in
      print_endline
        (Blist.to_string "\n" element_conv (CaseMap.to_list res))
    end ;
  let retval =
    only_first && first_pred_not_empty defs res ||
    not only_first &&
    CaseMap.for_all (fun _ s -> not (BasePairSet.is_empty s)) res in
  if retval then Stats.CC.accept () else Stats.CC.reject () ;
  retval

let of_formula pname params f defs =
  let head = (pname, params) in
  let caselist = Blist.map (fun h -> Sl_indrule.mk h head) f in
  (caselist, pname):: defs
