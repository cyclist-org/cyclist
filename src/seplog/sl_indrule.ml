open Lib
open Util
open Symbols
open MParser

include PairTypes(Sl_heap)(Sl_pred)

(* the normalisation of tags is used below to make sure that should we ever *)
(* try to create the same rule twice but with different tags, we get *)
(* structurally equal rules *)
let norm_tags h =
  let preds = Sl_tpreds.strip_tags (h.Sl_heap.inds) in
  let tag = ref 0 in
  let newinds = 
    Sl_pred.MSet.map_to 
      Sl_tpreds.add 
      Sl_tpreds.empty
      (fun p -> incr tag ; (!tag, p))
      preds in
  Sl_heap.with_inds h newinds

let mk_vars exist n = 
  let rec aux i = 
    if i>n then [] else 
    let newvar = 
      Sl_term.of_string 
        ((if exist then "v" else "u") ^ 
        (string_of_int i) ^ 
        (if exist then "'" else "")) in
    newvar::(aux (i+1)) in
  aux 1

let vars (f, (_, vs)) =
  Sl_term.Set.union (Sl_term.Set.of_list vs) (Sl_heap.vars f)
    
(* private version that does not go through mk *)
let _freshen varset ((h, (pred, args)) as case) =
  let casevars = vars case in
  let theta = Sl_term.avoid_theta varset casevars in
  (Sl_heap.subst theta h, (pred, Sl_term.FList.subst theta args))

(* normalize vars in rules to a fixed set *)
let norm_vars ((h', (_, args')) as case) =
  let no_evs = 
    Sl_term.Set.cardinal 
      (Sl_term.Set.filter Sl_term.is_exist_var (Sl_heap.vars h')) in
  (* assume non-arg vars are existentially quantified *) 
  let no_uvs = Blist.length args' in
  let evs' = mk_vars true no_evs in
  let uvs' = mk_vars false no_uvs in
  let varset = Sl_term.Set.of_list (evs' @ uvs') in
  let (h, (pred, args)) = _freshen varset case in
  let evs = 
    Sl_term.Set.to_list 
      (Sl_term.Set.filter Sl_term.is_exist_var (Sl_heap.vars h)) in
  let theta = 
    Sl_term.Map.of_list ((Blist.combine evs evs') @ (Blist.combine args uvs')) in
  (Sl_heap.subst theta h, (pred, Sl_term.FList.subst theta args))
  

let mk f i =
  let (_, args) = i in
  let v_args = Sl_term.Set.of_list args in
  let v_h = Sl_heap.terms f in
  let (uv_h, ev_h) = Sl_term.Set.partition Sl_term.is_univ_var v_h in
  assert (Blist.for_all Sl_term.is_univ_var args) ;
  assert (Sl_term.Set.cardinal v_args = Blist.length args) ;
  assert (Sl_term.Set.subset uv_h v_args) ;
  assert 
    (Sl_term.Set.for_all 
      (fun trm -> Sl_term.is_nil trm || Sl_term.is_exist_var trm) ev_h) ;   
  (norm_tags f, i)
  
let dest c = c

let predsym (_, pred) = Sl_pred.predsym pred
let arity (_, pred) = Sl_pred.arity pred
let formals (_, pred) = Sl_pred.args pred

let subst theta (f, (ident, args)) =
  let f = Sl_heap.subst theta f in
  let args = Sl_term.FList.subst theta args in
  
  let v_args = Sl_term.Set.of_list args in
  let v_h = Sl_heap.terms f in
  let (uv_h, ev_h) = Sl_term.Set.partition Sl_term.is_univ_var v_h in
  assert (Blist.for_all Sl_term.is_univ_var args) ;
  assert (Sl_term.Set.cardinal v_args = Blist.length args) ;
  assert (Sl_term.Set.subset uv_h v_args) ;
  assert 
    (Sl_term.Set.for_all 
      (fun trm -> Sl_term.is_nil trm || Sl_term.is_exist_var trm) ev_h) ;
  (f, (ident, args))   
  
let freshen varset case =
  let casevars = vars case in
  let theta = Sl_term.avoid_theta varset casevars in
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
  ( Sl_heap.parse >>= (fun h ->
          parse_symb symb_ind_implies >>
          Sl_pred.parse << spaces >>=
          (fun head -> return (mk h head))) <?> "case") st

let unfold vars h (tag, (ident, args)) case =
  let (f, (ident', formals)) = dest (freshen vars case) in
  assert (Sl_predsym.equal ident ident') ;
  let f = Sl_heap.freshen_tags h f in 
  let tagpairs = 
    Tags.map_to 
      TagPairs.add 
      TagPairs.empty 
      (fun tag' -> (tag,tag')) 
      (Sl_heap.tags f) in 
  let theta = Sl_term.Map.of_list (Blist.combine formals args) in
  Sl_heap.subst theta f, tagpairs
 
let fold (f, (predsym, args)) h =
  let vars = Sl_term.Set.of_list args in
  let tags = Sl_heap.tags h in
  let freshtag = 1 + (try Tags.max_elt tags with Not_found -> 0) in 
  let cont ((theta, _) as state) =
    (* disallow substituting over existentials in rule body *)
    Option.mk 
      (Sl_term.Map.for_all 
        (fun x y -> Sl_term.Set.mem x vars || Sl_term.is_exist_var y || true) 
        theta) 
      state in 
  let results = 
    Sl_term.backtrack 
      (Sl_heap.unify_partial ~tagpairs:true) 
      cont (Sl_term.empty_subst, TagPairs.empty) f h in
  Blist.map 
    (fun (theta, tagpairs) ->
      let f' = Sl_heap.subst_tags tagpairs (Sl_heap.subst theta f) in
      let newpred = (freshtag, (predsym, Sl_term.FList.subst theta args)) in
      let h' = 
        Sl_heap.mk
              (* FIXME hacky stuff in SH.eqs : in reality a proper way to diff *)
              (* two union-find structures is required *)
          (Sl_uf.of_list
            (Sl_deqs.to_list
              (Sl_deqs.diff
                (Sl_deqs.of_list (Sl_uf.bindings h.Sl_heap.eqs))
                (Sl_deqs.of_list (Sl_uf.bindings f'.Sl_heap.eqs))
              )))
          (Sl_deqs.diff h.Sl_heap.deqs f'.Sl_heap.deqs)
          (Sl_ptos.diff h.Sl_heap.ptos f'.Sl_heap.ptos)
          (Sl_tpreds.add
            newpred
            (Sl_tpreds.diff h.Sl_heap.inds f'.Sl_heap.inds)) in
      (theta, h')
    )
    results



