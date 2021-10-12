open Lib
open   Symbols

open Generic

open MParser

include Pair.Make (Pheap) (Pred)

let vars (f, (_, vs)) =
  Term.Set.union (Term.Set.of_list vs) (Pheap.vars f)

let mk f ((_, args) as hd) =
  let v_args = Term.Set.of_list args in
  let v_h = Pheap.terms f in
  let uv_h, ev_h = Term.Set.partition Term.is_free_var v_h in
  assert (Blist.for_all Term.is_free_var args) ;
  assert (Int.equal (Term.Set.cardinal v_args) (Blist.length args)) ;
  assert (Term.Set.subset uv_h v_args) ;
  assert (
    Term.Set.for_all
      (fun trm -> Term.is_nil trm || Term.is_exist_var trm)
      ev_h ) ;
  (f, hd)

let dest c = c

let predsym (_, pred) = Pred.predsym pred

let arity (_, pred) = Pred.arity pred

let formals (_, pred) = Pred.args pred

let body (h, _) = h

let subst theta (f, (ident, args)) =
  let f = Pheap.subst theta f in
  let args = Term.FList.subst theta args in
  let v_args = Term.Set.of_list args in
  let v_h = Pheap.terms f in
  let uv_h, ev_h = Term.Set.partition Term.is_free_var v_h in
  assert (Blist.for_all Term.is_free_var args) ;
  assert (Int.equal (Term.Set.cardinal v_args) (Blist.length args)) ;
  assert (Term.Set.subset uv_h v_args) ;
  assert (
    Term.Set.for_all
      (fun trm -> Term.is_nil trm || Term.is_exist_var trm)
      ev_h ) ;
  (f, (ident, args))

let freshen varset case =
  let casevars = vars case in
  let theta = Subst.avoid varset casevars in
  subst theta case

let pp fmt (f, (ident, vs)) =
  Format.fprintf fmt "@[%a%s%a%s%s%s@]" Pheap.pp f symb_ind_implies.sep
    Predsym.pp ident symb_lp.str
    (Blist.to_string "," Term.to_string vs)
    symb_rp.str

let to_string c = mk_to_string pp c

let parse st =
  ( Pheap.parse ~allow_tags:false
  >>= (fun h ->
        parse_symb symb_ind_implies
        >> Pred.parse << spaces
        >>= fun head -> return (mk h head) )
  <?> "case" )
    st

let unfold ?(gen_tags = true) (vars, tags) (tag, (ident, args)) case =
  let f, (ident', formals) = dest (freshen vars case) in
  assert (Predsym.equal ident ident') ;
  assert (Blist.length args == Blist.length formals) ;
  assert (Tags.is_empty (Pheap.tags f)) ;
  let f = if gen_tags then Pheap.complete_tags tags f else f in
  let theta = Term.Map.of_list (Blist.combine formals args) in
  Pheap.subst theta f

(* TODO: check whether the update check is necessary for what this function is being used for *)
let fold (f, (predsym, args)) h =
  let freshtag = Tags.fresh_fvar (Pheap.tags h) in
  let results =
    Unify.Unidirectional.realize
      (Unification.backtrack
         (Pheap.unify_partial ~tagpairs:true
            ~update_check:Unify.Unidirectional.trm_check)
         f h Unification.trivial_continuation)
  in
  let do_fold (theta, tagpairs) =
    let f' = Pheap.subst_tags tagpairs (Pheap.subst theta f) in
    let newpred = (freshtag, (predsym, Term.FList.subst theta args)) in
    let h' = Pheap.add_ind (Pheap.diff h f') newpred in
    (theta, h')
  in
  Blist.map do_fold results

let memory_consuming (h, _) = Pheap.memory_consuming h

let constructively_valued (h, _) = Pheap.constructively_valued h
