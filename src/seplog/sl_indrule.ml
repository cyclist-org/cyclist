open Lib

open Symbols
open MParser

include Pair.Make(Sl_heap)(Sl_pred)

let vars (f, (_, vs)) =
  Sl_term.Set.union (Sl_term.Set.of_list vs) (Sl_heap.vars f)
    
let mk f ((_, args) as hd) =
  let v_args = Sl_term.Set.of_list args in
  let v_h = Sl_heap.terms f in
  let (uv_h, ev_h) = Sl_term.Set.partition Sl_term.is_free_var v_h in
  assert (Blist.for_all Sl_term.is_free_var args) ;
  assert (Sl_term.Set.cardinal v_args = Blist.length args) ;
  assert (Sl_term.Set.subset uv_h v_args) ;
  assert 
    (Sl_term.Set.for_all 
      (fun trm -> Sl_term.is_nil trm || Sl_term.is_exist_var trm) ev_h) ;   
  (f, hd)
  
let dest c = c

let predsym (_, pred) = Sl_pred.predsym pred
let arity (_, pred) = Sl_pred.arity pred
let formals (_, pred) = Sl_pred.args pred
let body (h, _) = h

let subst theta (f, (ident, args)) =
  let f = Sl_heap.subst theta f in
  let args = Sl_term.FList.subst theta args in
  
  let v_args = Sl_term.Set.of_list args in
  let v_h = Sl_heap.terms f in
  let (uv_h, ev_h) = Sl_term.Set.partition Sl_term.is_free_var v_h in
  assert (Blist.for_all Sl_term.is_free_var args) ;
  assert (Sl_term.Set.cardinal v_args = Blist.length args) ;
  assert (Sl_term.Set.subset uv_h v_args) ;
  assert 
    (Sl_term.Set.for_all 
      (fun trm -> Sl_term.is_nil trm || Sl_term.is_exist_var trm) ev_h) ;
  (f, (ident, args))   
  
let freshen varset case =
  let casevars = vars case in
  let theta = Sl_subst.avoid varset casevars in
  subst theta case

let pp fmt (f, (ident, vs)) =
  Format.fprintf fmt "@[%a%s%a%s%s%s@]"
    Sl_heap.pp f
    symb_ind_implies.sep
    Sl_predsym.pp ident
    symb_lp.str
    (Blist.to_string "," Sl_term.to_string vs)
    symb_rp.str

let to_string c = mk_to_string pp c

let parse st =
  ( (Sl_heap.parse ~allow_tags:false) >>= (fun h ->
          parse_symb symb_ind_implies >>
          Sl_pred.parse << spaces >>=
          (fun head -> return (mk h head))) <?> "case") st

let unfold ?(gen_tags=true) (vars, tags) (tag, (ident, args)) case =
  let (f, (ident', formals)) = dest (freshen vars case) in
  assert (Sl_predsym.equal ident ident') ;
  assert (Blist.length args == Blist.length formals) ;
  assert (Tags.is_empty (Sl_heap.tags f)) ;
  let f = if gen_tags then Sl_heap.complete_tags tags f else f in 
  let theta = Sl_term.Map.of_list (Blist.combine formals args) in
  Sl_heap.subst theta f

(* TODO: check whether the update check is necessary for what this function is being used for *) 
let fold (f, (predsym, args)) h =
  let freshtag = Tags.fresh_fvar (Sl_heap.tags h) in 
  let results =
    Sl_unify.Unidirectional.realize
      (Unification.backtrack 
        (Sl_heap.unify_partial 
          ~tagpairs:true ~update_check:Sl_unify.Unidirectional.trm_check)
        f h
        Unification.trivial_continuation) in
  let do_fold (theta, tagpairs) =
    let f' = Sl_heap.subst_tags tagpairs (Sl_heap.subst theta f) in
    let newpred = (freshtag, (predsym, Sl_term.FList.subst theta args)) in
    let h' = Sl_heap.add_ind (Sl_heap.diff h f') newpred in
    (theta, h') in
  Blist.map do_fold results

let memory_consuming (h, _) = Sl_heap.memory_consuming h
let constructively_valued (h, _) = Sl_heap.constructively_valued h
