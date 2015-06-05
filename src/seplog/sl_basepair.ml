open Lib
open Util
open Symbols
open MParser

module SH = Sl_heap

module AllocatedT = PairTypes(Sl_term)(Int.T)  
module Allocated = 
  struct
    include MakeListSet(AllocatedT)
    
    let terms s = map_to Sl_term.Set.add Sl_term.Set.empty fst s
    let vars s = Sl_term.filter_vars (terms s)
    let endomap_fst f s = endomap (fun (x,i) -> (f x, i)) s
    
  end 


module BasePair =
struct
  include PairTypes(Allocated)(Sl_heap)
  
  let to_string (v, g) =
    symb_lp.str ^
    symb_lb.str ^
    (Blist.to_string 
      symb_comma.sep 
      AllocatedT.to_string 
      (Allocated.to_list v)) ^
    symb_rb.str ^ symb_comma.sep ^
    (Sl_heap.to_string g) ^
    symb_rp.str
  
  let pp fmt (v, g) = 
    Format.fprintf fmt "@[%s%s%a%s%s%a%s@]"
      symb_lp.str
      symb_lb.str
      (Blist.pp pp_commasp AllocatedT.pp) (Allocated.to_list v)
      symb_rb.str 
      symb_comma.sep
      Sl_heap.pp g
      symb_rp.str
  
  let vars (v, h) = Sl_term.Set.union (Allocated.vars v) (Sl_heap.vars h)
  
  let norm (v, h) =
    let h' = Sl_heap.norm h in 
    let v' = 
      Allocated.endomap_fst (fun x -> Sl_uf.find x h'.Sl_heap.eqs) v in
    (v', h')    
  
  (* pre: g is consistent *)
  let project (v, g) case =
    let formals = Sl_indrule.formals case in
    let formals_set = Sl_term.Set.of_list formals in
    let g' = Sl_heap.project g formals in
    let v' = 
      Allocated.filter 
        (fun (x,_) -> Sl_term.Set.mem x formals_set) v in
    norm (v', g')
  
  let subst theta (v, g) =
    let v' = 
      Allocated.endomap_fst (fun z -> Sl_term.subst theta z) v in
    let g' = Sl_heap.subst theta g in
    (v', g')
  
  let unfold (v, h) (_, (_, params)) (case, (v', g')) =
    (* simultaneously freshen case and (v',g') *)
    let avoidvars = 
      Sl_term.Set.union (Allocated.vars v) (Sl_heap.vars h) in
    let theta = Sl_term.avoid_theta avoidvars (Sl_indrule.vars case) in
    let case = Sl_indrule.subst theta case in
    let (v', g') = subst theta (v', g') in
    (* now carry on with substitution as normal *)
    let formals = Sl_indrule.formals case in
    let theta = Sl_term.Map.of_list (Blist.combine formals params) in
    let (v', g') = subst theta (v', g') in
    (* let h' = SH.del_ind h ind in *)
    let h' = Sl_heap.star h g' in
    let cv = 
      Blist.cartesian_product 
        (Allocated.map_to_list fst v) 
        (Allocated.map_to_list fst v') in
    let h' = 
      SH.with_deqs h' (Sl_deqs.union h'.SH.deqs (Sl_deqs.of_list cv)) in
    let v = Allocated.union v v' in
    (v, h')
  
  (* assumes case is built with Sl_heap.star so ys are already unequal *)
  let unfold_all case cbps =
    let (h, _) = Sl_indrule.dest case in
    (* let () = assert (Sl_tpreds.cardinal h.inds = Blist.length cbps) in *)
    let ys = 
      Sl_ptos.map_to 
        Allocated.add 
        Allocated.empty 
        Sl_pto.record_type 
        h.SH.ptos in
    let h' = SH.with_ptos h Sl_ptos.empty in
    let h' = SH.with_inds h' Sl_tpreds.empty in
    Blist.fold_left2 unfold (ys, h') (Sl_tpreds.to_list h.SH.inds) cbps
  
  let gen case cbps =
    let args = Sl_indrule.formals case in
    let (v, h) = unfold_all case cbps in
    if Sl_heap.inconsistent h then None else
    let allvars =
      Blist.rev_append (Sl_term.Set.to_list (Sl_heap.vars h)) args in
    let v' = 
      Allocated.fold 
        (fun (x,i) v'' ->
          let equals = Blist.rev_filter (Sl_heap.equates h x) allvars in
          let pairs = Blist.rev_map (fun y -> (y,i)) equals in
          Allocated.union v'' (Allocated.of_list pairs)
        )
        v 
        Allocated.empty in
    (* let l =                                                                   *)
    (*   Blist.rev_append (Sl_term.Set.to_list (Sl_heap.vars h)) args in         *)
    (* let l =                                                                   *)
    (*   Blist.rev_filter                                                        *)
    (*     (fun u -> Sl_term.Set.exists (fun z -> Sl_heap.equates h u z) v) l in *)
    (* let v = Sl_term.Set.of_list l in                                          *)
    Some (project (v', h) case)
  
  let leq (v,h) (v',h') = 
    Sl_heap.subsumed h h' 
    &&
    let (v,v') = 
      (* use stronger heap to rewrite both variable sets *)
      Pair.map 
        (Allocated.endomap_fst (fun x -> Sl_uf.find x h'.Sl_heap.eqs)) 
        (v,v') in
    Allocated.subset v v'
 
end

include BasePair

module Set = MakeTreeSet(BasePair)
module Hashset = Hashset.Make(BasePair)

module RuleMap = MakeMap(Sl_indrule)

let get_bps cmap (_, (ident, _)) =
  RuleMap.fold
    (fun c s l -> 
      if Sl_predsym.equal ident (fst (snd (Sl_indrule.dest c))) 
      then 
        Hashset.fold (fun bp l' -> (c, bp)::l') s l
      else
        l
    ) 
    cmap
    []

(* let tried = Hashset.create 997 *)

let gen_pairs case cmap =
  let (h, _) = Sl_indrule.dest case in
  let candidates = Sl_tpreds.map_to_list (fun i -> get_bps cmap i) h.SH.inds in
  let l = Blist.choose candidates in
  (* let l = Blist.rev_filter (fun cbp -> not (Hashset.mem tried (case, cbp))) l in *)
  (* let () = Blist.iter (fun cbp -> Hashset.add tried (case, cbp)) l in            *)
  let poss_bps = Blist.rev_map (fun cbps -> gen case cbps) l in
  let bps = Option.list_get poss_bps in
  bps

let first_pred_not_empty defs cm =
  let defs = Sl_defs.to_list defs in
  let first_pred = Sl_preddef.predsym (Blist.hd defs) in
    RuleMap.exists
      (fun k v ->
        Sl_predsym.equal (Sl_indrule.predsym k) first_pred &&
        not (Hashset.is_empty v)
      )
      cm

let gen_all_pairs ?(only_first=false) defs =
  (* let () = Hashset.clear tried in *)
  let first_not_empty c = first_pred_not_empty defs c in
  let defs = Sl_defs.to_list defs in
  let cmap =
    Blist.fold_left
      (fun m d ->
        Blist.fold_left
          (fun m' c -> RuleMap.add c (Hashset.create 11) m') m (Sl_preddef.rules d))
      RuleMap.empty
      defs in
  let add_check s x =
    let notin = not (Hashset.mem s x) in
    if notin then Hashset.add s x ;
    notin in
  let rec onestep () =
    let progress = 
      RuleMap.fold
        (fun c s prog -> 
          let bps = gen_pairs c cmap in
          Blist.fold_left
            (fun prog' bp -> 
              let new_prog = add_check s bp in
              if only_first && first_not_empty cmap then 
                false
              else
                new_prog || prog')
            prog
            bps
        )
        cmap
        false
        in
    let () = debug (fun () -> "\n" ^ (RuleMap.to_string Hashset.to_string cmap) ^ "\n") in
    if progress then onestep () else () in
  let _ = onestep() in
  cmap
  (* let rec fixp cm =                                                    *)
  (*   let (cm', progress) = onestep cm in                                *)
  (*   if only_first && first_not_empty cm' || not progress then cm' else *)
  (*     fixp cm' in                                                      *)
  (* fixp cmap                                                            *)

(* NB correctness relies on rules being explicit about x->_ implying       *)
(* x!=nil !!!                                                              *)
let satisfiable ?(only_first=false) ?(output=false) defs =
  Stats.CC.call () ;
  let res = gen_all_pairs ~only_first defs in
  if output then
    begin
      let element_conv (c, s) =
        ((Sl_indrule.to_string c) ^ " has base " ^ (Hashset.to_string s)) in
      print_endline
        (Blist.to_string "\n" element_conv (RuleMap.to_list res))
    end ;
  let retval =
    only_first && first_pred_not_empty defs res ||
    not only_first && RuleMap.for_all (fun _ s -> not (Hashset.is_empty s)) res in
  if retval then Stats.CC.accept () else Stats.CC.reject () ;
  retval

let form_sat defs f = satisfiable ~only_first:true (Sl_defs.of_formula defs f)

let pairs_of_form defs f =
  let defs = Sl_defs.of_formula defs f in
  let bp_map = gen_all_pairs defs in
  let (rules,_) = Sl_preddef.dest (Blist.hd (Sl_defs.to_list defs)) in
  let s = Hashset.create 11 in 
  let () = Blist.iter
    (fun r -> let _ = Hashset.left_union s (RuleMap.find r bp_map) in ()) 
    rules in
  Hashset.map_to Set.add Set.empty Fun.id s
  
let minimise bps = 
  let res = 
    Set.fold 
      (fun x bps' ->
        if Set.exists (fun y -> BasePair.leq y x) bps' then 
          bps'
        else
          Set.add x bps' 
        ) 
      bps
      Set.empty in
  (* if not (Set.equal res bps) then print_endline "~" ;  *)
  res 
  
    