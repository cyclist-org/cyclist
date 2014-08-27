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
    let g' = Sl_heap.project g formals in
    let proj lval =
      if Sl_heap.equates g' lval Sl_term.nil then 
        Sl_term.nil 
      else
        Sl_uf.find lval g'.Sl_heap.eqs in  
    let v' =       
      Sl_term.Set.endomap  
        proj  
        (Sl_term.Set.inter v (Sl_term.Set.of_list formals)) in
    (v', g')
  
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
    let formals = Sl_indrule.formals case in
    let theta = Sl_term.Map.of_list (Blist.combine formals params) in
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
    let ys = 
      Sl_ptos.map_to Sl_term.Set.add Sl_term.Set.empty fst h.SH.ptos in
    let h = SH.with_ptos h Sl_ptos.empty in
    Blist.fold_left2 unfold (ys, h) (Sl_tpreds.to_list h.SH.inds) cbps
  
  let gen case cbps =
    let args = Sl_indrule.formals case in
    let (v, h) = unfold_all case cbps in
    if Sl_heap.inconsistent h then None else
    let l = Blist.rev_append (Sl_term.Set.to_list (Sl_heap.vars h)) args in
    let l = Blist.rev_filter
        (fun u -> Sl_term.Set.exists (fun z -> Sl_heap.equates h u z) v) l in
    let v = Sl_term.Set.of_list l in
    Some (project (v, h) case)
 
end

include BasePair

module Set = MakeTreeSet(BasePair)

module RuleMap = MakeMap(Sl_indrule)

let get_bps cmap (_, (ident, _)) =
  Blist.rev 
    (RuleMap.fold
      (fun c s l -> 
        if Sl_predsym.equal ident (fst (snd (Sl_indrule.dest c))) 
        then 
          Set.fold (fun bp l' -> (c, bp)::l') s l 
        else 
          l
      ) 
      cmap
      [])

let gen_pairs case cmap =
  let (h, _) = Sl_indrule.dest case in
  let candidates = Sl_tpreds.map_to_list (fun i -> get_bps cmap i) h.SH.inds in
  let l = Blist.choose candidates in
  let poss_bps = Blist.rev_map (fun cbps -> gen case cbps) l in
  let bps = Option.list_get poss_bps in
  Set.of_list bps

let first_pred_not_empty defs =
  let defs = Sl_defs.to_list defs in
  let first_pred = Sl_preddef.predsym (Blist.hd defs) in
  fun cm ->
    RuleMap.exists
      (fun k v ->
        Sl_predsym.equal (Sl_indrule.predsym k) first_pred &&
        not (Set.is_empty v)
      )
      cm

let gen_all_pairs ?(only_first=false) defs =
  let first_not_empty = first_pred_not_empty defs in
  let defs = Sl_defs.to_list defs in
  let cmap =
    Blist.fold_left
      (fun m d ->
        Blist.fold_left
          (fun m' c -> RuleMap.add c Set.empty m') m (Sl_preddef.rules d))
      RuleMap.empty
      defs
  in
  let onestep cmap =
    let progress = ref false in
    let r = RuleMap.endomap
      (fun (c, s) -> 
        let n = Set.cardinal s in
        let s' = Set.union s (gen_pairs c cmap) in
        let n' = Set.cardinal s' in
        progress := !progress || n'>n; 
        (c, s'))
      cmap in
    let () = debug (fun () -> "\n" ^ (RuleMap.to_string Set.to_string r) ^ "\n") in
    (!progress, r) in
  let rec fixp cm =
    let (progress, cm') = onestep cm in
    if
      only_first && first_not_empty cm' || not progress
    then
      cm'
    else
      fixp cm' in
  fixp cmap

(* NB correctness relies on rules being explicit about x->_ implying       *)
(* x!=nil !!!                                                              *)
let satisfiable ?(only_first=false) ?(output=false) defs =
  Stats.CC.call () ;
  let res = gen_all_pairs ~only_first defs in
  if output then
    begin
      let element_conv (c, s) =
        ((Sl_indrule.to_string c) ^ " has base " ^ (Set.to_string s)) in
      print_endline
        (Blist.to_string "\n" element_conv (RuleMap.to_list res))
    end ;
  let retval =
    only_first && first_pred_not_empty defs res ||
    not only_first &&
    RuleMap.for_all (fun _ s -> not (Set.is_empty s)) res in
  if retval then Stats.CC.accept () else Stats.CC.reject () ;
  retval

let form_sat defs f = satisfiable ~only_first:true (Sl_defs.of_formula defs f)

let pairs_of_form defs f =
  let defs = Sl_defs.of_formula defs f in
  let bp_map = gen_all_pairs defs in
  let (rules,_) = Sl_preddef.dest (Blist.hd (Sl_defs.to_list defs)) in
  Blist.foldl 
    (fun bps r -> Set.union bps (RuleMap.find r bp_map)) 
    Set.empty 
    rules
  
