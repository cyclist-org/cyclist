open Lib
open Util
open Symbols
open MParser

include PairTypes(Sld_heap)(Sld_pred)

(* module Var = Hashtbl.Make(Int.T)                        *)
  
(* let freevars = Var.create 997                           *)
(* let existvars = Var.create 997                          *)

(* let mk_var exist i =                                    *)
(*   let varset = if exist then existvars else freevars in *)
(*   try                                                   *)
(*     Var.find varset i                                   *)
(*   with Not_found ->                                     *)
(*     let newvar =                                        *)
(*       Sld_term.of_string                                 *)
(*         ((if exist then "v" else "u") ^                 *)
(*         (string_of_int i) ^                             *)
(*         (if exist then "'" else "")) in                 *)
(*     Var.add varset i newvar ; newvar                    *)

(* let mk_exist_var i = mk_var true i                      *)
(* let mk_free_var i = mk_var false i                      *)

let vars (f, (_, vs)) =
  Sld_term.Set.union (Sld_term.Set.of_list vs) (Sld_heap.vars f)
    
(* private version that does not go through mk *)
let _freshen varset ((h, (pred, args)) as case) =
  let casevars = vars case in
  let theta = Sld_term.avoid_theta varset casevars in
  (Sld_heap.subst theta h, (pred, Sld_term.FList.subst theta args))

(* normalize vars in rules to a fixed set *)
(* let norm_vars ((h', (_, args')) as case) =                                        *)
(*   (* assume non-arg vars are existentially quantified *)                          *)
(*   let evs =                                                                       *)
(*     Sld_term.Set.to_list                                                           *)
(*       (Sld_term.Set.filter Sld_term.is_exist_var (Sld_heap.vars h')) in              *)
(*   let evs' = Blist.mapi (fun i _ -> mk_exist_var i) evs in                        *)
(*   let fvs' = Blist.mapi (fun i _ -> mk_free_var i) args' in                       *)
(*   let varset = Sld_term.Set.of_list (evs' @ fvs') in                               *)
(*   let (h, (pred, args)) = _freshen varset case in                                 *)
(*   let evs =                                                                       *)
(*     Sld_term.Set.to_list                                                           *)
(*       (Sld_term.Set.filter Sld_term.is_exist_var (Sld_heap.vars h)) in               *)
(*   let theta =                                                                     *)
(*     Sld_term.Map.of_list ((Blist.combine evs evs') @ (Blist.combine args fvs')) in *)
(*   (Sld_heap.subst theta h, (pred, Sld_term.FList.subst theta args))                 *)
  
(* the normalisation of tags is used below to make sure that should we ever *)
(* try to create the same rule twice but with different tags, we get *)
(* structurally equal rules *)
let norm_tags h =
  let preds = Sld_tpreds.strip_tags (h.Sld_heap.inds) in
  let newinds = 
    Sld_tpreds.of_list 
      (Blist.mapi (fun tag p -> (1+tag, p)) (Sld_pred.MSet.to_list preds)) in
  Sld_heap.with_inds h newinds

let mk f i =
  let (_, args) = i in
  let v_args = Sld_term.Set.of_list args in
  let v_h = Sld_heap.terms f in
  let (uv_h, ev_h) = Sld_term.Set.partition Sld_term.is_univ_var v_h in
  assert (Blist.for_all Sld_term.is_univ_var args) ;
  assert (Sld_term.Set.cardinal v_args = Blist.length args) ;
  assert (Sld_term.Set.subset uv_h v_args) ;
  assert 
    (Sld_term.Set.for_all 
      (fun trm -> Sld_term.is_nil trm || Sld_term.is_exist_var trm) ev_h) ;   
  (norm_tags f, i)
  
let dest c = c

let predsym (_, pred) = Sld_pred.predsym pred
let arity (_, pred) = Sld_pred.arity pred
let formals (_, pred) = Sld_pred.args pred
let body (h, _) = h

let subst theta (f, (ident, args)) =
  let f = Sld_heap.subst theta f in
  let args = Sld_term.FList.subst theta args in
  
  let v_args = Sld_term.Set.of_list args in
  let v_h = Sld_heap.terms f in
  let (uv_h, ev_h) = Sld_term.Set.partition Sld_term.is_univ_var v_h in
  assert (Blist.for_all Sld_term.is_univ_var args) ;
  assert (Sld_term.Set.cardinal v_args = Blist.length args) ;
  assert (Sld_term.Set.subset uv_h v_args) ;
  assert 
    (Sld_term.Set.for_all 
      (fun trm -> Sld_term.is_nil trm || Sld_term.is_exist_var trm) ev_h) ;
  (f, (ident, args))   
  
let freshen varset case =
  let casevars = vars case in
  let theta = Sld_term.avoid_theta varset casevars in
  subst theta case

let pp fmt (f, (ident, vs)) =
  Format.fprintf fmt "@[%a%s%a%s%s%s@]"
    Sld_heap.pp f
    symb_ind_implies.sep
    Sld_predsym.pp ident
    symb_lp.str
    (Blist.to_string "," Sld_term.to_string vs)
    symb_rp.str

let to_string c = mk_to_string pp c

let parse st =
  ( Sld_heap.parse >>= (fun h ->
          parse_symb symb_ind_implies >>
          Sld_pred.parse << spaces >>=
          (fun head -> return (mk h head))) <?> "case") st

let unfold vars h (tag, (ident, args)) case =
  let (f, (ident', formals)) = dest (freshen vars case) in
  assert (Sld_predsym.equal ident ident') ;
  assert (Blist.length args == Blist.length formals) ;
  let f = Sld_heap.freshen_tags h f in 
  let tagpairs = 
    Tags.map_to 
      TagPairs.add 
      TagPairs.empty 
      (fun tag' -> (tag,tag')) 
      (Sld_heap.tags f) in 
  let theta = Sld_term.Map.of_list (Blist.combine formals args) in
  Sld_heap.subst theta f, tagpairs
 
let fold (f, (predsym, args)) h =
  let vars = Sld_term.Set.of_list args in
  let tags = Sld_heap.tags h in
  let freshtag = 1 + (try Tags.max_elt tags with Not_found -> 0) in 
  let sub_check = Sld_term.combine_subst_checks [
      Sld_term.basic_lhs_down_check ;
      (fun _ x y -> Sld_term.Set.mem x vars || Sld_term.is_exist_var y || true) ; 
    ] in 
  let results = 
    Sld_term.backtrack 
      (Sld_heap.unify_partial ~tagpairs:true)
      ~sub_check
      f h in
  let do_fold (theta, tagpairs) =
    let f' = Sld_heap.subst_tags tagpairs (Sld_heap.subst theta f) in
    let newpred = (freshtag, (predsym, Sld_term.FList.subst theta args)) in
    let h' = Sld_heap.add_ind (Sld_heap.diff h f') newpred in
    (theta, h') in
  Blist.map do_fold results
