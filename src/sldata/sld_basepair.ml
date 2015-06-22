open Lib
open Util
open Symbols
open MParser

module SH = Sld_heap

module AllocatedT = PairTypes(Sld_term)(Int.T)  
module Allocated = 
  struct
    include MakeTreeSet(AllocatedT)
    
    let terms s = map_to Sld_term.Set.add Sld_term.Set.empty fst s
    let vars s = Sld_term.filter_vars (terms s)
    let endomap_fst f s = endomap (fun (x,i) -> (f x, i)) s  
  end 


module BasePair =
struct
  module T = PairTypes(Allocated)(Sld_heap)
  include T
  include ContaineriseType(T)
  
  let to_string (v, g) =
    symb_lp.str ^
    symb_lb.str ^
    (Blist.to_string 
      symb_comma.sep 
      AllocatedT.to_string 
      (Allocated.to_list v)) ^
    symb_rb.str ^ symb_comma.sep ^
    (Sld_heap.to_string g) ^
    symb_rp.str
  
  let pp fmt (v, g) = 
    Format.fprintf fmt "@[%s%s%a%s%s%a%s@]"
      symb_lp.str
      symb_lb.str
      (Blist.pp pp_commasp AllocatedT.pp) (Allocated.to_list v)
      symb_rb.str 
      symb_comma.sep
      Sld_heap.pp g
      symb_rp.str
  
  let vars (v, h) = Sld_term.Set.union (Allocated.vars v) (Sld_heap.vars h)
  
  let norm (v, h) =
    let h' = Sld_heap.norm h in 
    let v' = 
      Allocated.endomap_fst (fun x -> Sld_uf.find x h'.Sld_heap.eqs) v in
    (v', h')    
  
  (* pre: g is consistent *)
  let project (v, g) case =
    let formals = Sld_indrule.formals case in
    let formals_set = Sld_term.Set.of_list formals in
    let g' = Sld_heap.project g formals in
    let v' = 
      Allocated.filter 
        (fun (x,_) -> Sld_term.Set.mem x formals_set) v in
    norm (v', g')
  
  let subst theta (v, g) =
    let v' = 
      Allocated.endomap_fst (fun z -> Sld_term.subst theta z) v in
    let g' = Sld_heap.subst theta g in
    (v', g')
  
  let unfold (v, h) (_, (_, params)) ((v', g'), formals) =
    (* simultaneously freshen case and (v',g') *)
    let avoidvars = 
      Sld_term.Set.union (Allocated.vars v) (Sld_heap.vars h) in
    let theta = Sld_term.avoid_theta avoidvars (Sld_term.Set.of_list formals) in
    (* let theta = Sld_term.avoid_theta avoidvars (Sld_indrule.vars case) in *)
    let formals = Blist.map (fun x -> Sld_term.subst theta x) formals in
    let (v', g') = subst theta (v', g') in
    (* now carry on with substitution as normal *)
    (* let formals = Sld_indrule.formals case in *)
    let theta = Sld_term.Map.of_list (Blist.combine formals params) in
    let (v', g') = subst theta (v', g') in
    let h' = Sld_heap.star h g' in
    let deqs = 
      Allocated.fold
        (fun (x,_) deqs ->
          Allocated.fold
            (fun (y,_) deqs' -> Sld_deqs.add (x,y) deqs')
            v'
            deqs
        )
        v
        h'.SH.deqs in
    let h' = SH.with_deqs h' deqs in
    let v = Allocated.union v v' in
    (v, h')
  
  (* assumes case is built with Sld_heap.star so ys are already unequal *)
  let unfold_all case cbps =
    let (h, _) = Sld_indrule.dest case in
    (* let () = assert (Sld_tpreds.cardinal h.inds = Blist.length cbps) in *)
    let ys = 
      Sld_ptos.map_to 
        Allocated.add 
        Allocated.empty 
        Sld_pto.record_type 
        h.SH.ptos in
    let h' = SH.with_ptos h Sld_ptos.empty in
    let h' = SH.with_inds h' Sld_tpreds.empty in
    Blist.fold_left2 unfold (ys, h') (Sld_tpreds.to_list h.SH.inds) cbps
  
  let gen case cbps =
    let args = Sld_indrule.formals case in
    let (v, h) = unfold_all case cbps in
    if Sld_heap.inconsistent h then None else
    let allvars =
      Blist.rev_append (Sld_term.Set.to_list (Sld_heap.vars h)) args in
    let v' = 
      Allocated.fold 
        (fun (x,i) v'' ->
          let equals = Blist.rev_filter (Sld_heap.equates h x) allvars in
          Blist.map_to Allocated.add v'' (fun y -> (y,i)) equals
        )
        v 
        Allocated.empty in
    Some (project (v', h) case)
  
  let leq (v,h) (v',h') = 
    Sld_heap.subsumed h h' 
    &&
    let (v,v') = 
      (* use stronger heap to rewrite both variable sets *)
      Pair.map 
        (Allocated.endomap_fst (fun x -> Sld_uf.find x h'.Sld_heap.eqs)) 
        (v,v') in
    Allocated.subset v v'
 
end

module BaseAndRule = 
struct
  module T = PairTypes(BasePair)(Sld_term.FList)
  include T
  include ContaineriseType(T)
end

module Attempted = Hashset.Make(BaseAndRule.FList)

include BasePair

module RuleMap = MakeMap(Sld_indrule)
module PredMap = MakeMap(Sld_predsym)


let gen_pair case cbps s s' att = 
  if Attempted.mem att cbps then () else 
  match gen case cbps with
  | None -> ()
  | Some bp -> 
    Hashset.add s bp ; 
    BaseAndRule.Hashset.add s' (bp, Sld_indrule.formals case) ; 
    Attempted.add att cbps

let choose_iter f ys = 
  let rec aux f acc = function
    | [] -> f acc
    | xs::tl -> BaseAndRule.Hashset.iter (fun x -> aux f (x::acc) tl) xs in
  aux f [] (Blist.rev ys)  
  
(* the f argument is a unit-taking function that checks if we can stop *)
(* due to first predicate being non-empty *)
let gen_pairs f case cmap pmap attmap =
  let (h, (pred,_)) = Sld_indrule.dest case in
  let candidates = 
    Sld_tpreds.map_to_list (fun (_,(i,_)) -> PredMap.find i pmap) h.SH.inds in
  let s = RuleMap.find case cmap in
  let s' = PredMap.find pred pmap in 
  let att = RuleMap.find case attmap in
  choose_iter
    (fun cbps -> f () ; gen_pair case cbps s s' att)
    candidates 

let first_pred_not_empty defs =
  let defs = Sld_defs.to_list defs in
  let first_pred = Sld_preddef.predsym (Blist.hd defs) in
  fun pmap ->
    not (BaseAndRule.Hashset.is_empty (PredMap.find first_pred pmap))

exception FirstNonEmpty

let gen_all_pairs ?(only_first=false) defs =
  let first_not_empty = first_pred_not_empty defs in
  let defs = Sld_defs.to_list defs in
  let cmap =
    Blist.fold_left
      (fun m d ->
        Blist.fold_left
          (fun m' c -> RuleMap.add c (Hashset.create 11) m') 
          m 
          (Sld_preddef.rules d)
      )
      RuleMap.empty
      defs in
  let get_sizemap cmap =
    RuleMap.fold 
      (fun c s m -> RuleMap.add c (Hashset.cardinal s) m) cmap RuleMap.empty in 
  let pmap =
    Blist.fold_left
      (fun m d ->
        let first = Blist.hd (Sld_preddef.rules d) in
        PredMap.add (Sld_indrule.predsym first) (BaseAndRule.Hashset.create 11) m
      )
      PredMap.empty
      defs in
  let att = 
    Blist.fold_left
      (fun m d ->
        Blist.fold_left
          (fun m' c -> RuleMap.add c (Attempted.create 11) m') 
          m 
          (Sld_preddef.rules d)
      )
      RuleMap.empty
      defs in
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
        debug 
          (fun () -> "\n" ^ (RuleMap.to_string Hashset.to_string cmap) ^ "\n") ;
        RuleMap.iter (fun c _ -> gen_pairs f c cmap pmap att) cmap ;
        let newsizemap = get_sizemap cmap in
        progress := not (RuleMap.equal Int.equal !sizemap newsizemap) ;
        sizemap := newsizemap 
      done
    with FirstNonEmpty -> () in
  (cmap, pmap)

(* NB correctness relies on rules being explicit about x->_ implying       *)
(* x!=nil !!!                                                              *)
let satisfiable ?(only_first=false) ?(output=false) defs =
  Stats.CC.call () ;
  let (cmap, pmap) = gen_all_pairs ~only_first defs in
  if output then
    begin
      let element_conv (c, s) =
        ((Sld_indrule.to_string c) ^ " has base " ^ (Hashset.to_string s)) in
      print_endline
        (Blist.to_string "\n" element_conv (RuleMap.to_list cmap))
    end ;
  let retval =
    only_first && first_pred_not_empty defs pmap ||
    not only_first && RuleMap.for_all (fun _ s -> not (Hashset.is_empty s)) cmap in
  if retval then Stats.CC.accept () else Stats.CC.reject () ;
  retval

let form_sat defs f = satisfiable ~only_first:true (Sld_defs.of_formula defs f)

let pairs_of_form defs f =
  let defs = Sld_defs.of_formula defs f in
  let (bp_map, _) = gen_all_pairs defs in
  let (rules,_) = Sld_preddef.dest (Blist.hd (Sld_defs.to_list defs)) in
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
  
    
