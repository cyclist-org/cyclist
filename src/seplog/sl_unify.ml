module type S = 
sig
    
  type state
  (** State maintained by unifiers. *)
  
  val empty_state : state
  (** The unifier state consisting of the empty substitution and the empty set of
      tag pairs *)
      
  type continuation = (state, state) Unification.continuation 
  (** The type of continuations accepted by SL cps-unifiers *)
  
  type 'a unifier = (state, state, 'a) Unification.cps_unifier
  (** The type of SL unifiers - these always act on pairs of term substitutions
      and tagpair sets, and only accept continuations that are maps on such 
      pairs.
  *)
  
  val realize : (state, 'a) Unification.realizer
  
  type state_check = state Fun.predicate
  (** Predicates that check unifier states for validity *)
  
  val mk_assert_check : state_check -> state_check
  (** Takes a state check and wraps it in an assert *)
  
  val mk_verifier : state_check -> continuation
  (** Takes a state check function and converts it into a continuation which
      returns None if the check fails *)
  
  type update_check = (state Unification.state_update) Fun.predicate
  
  val unify_tag : ?update_check:update_check -> Tags.Elt.t unifier
  val unify_trm : ?update_check:update_check -> Sl_term.t unifier
  val unify_trm_list : ?update_check:update_check -> Sl_term.FList.t unifier
end

module Make(Core : sig type state val empty_state : state end) =
struct
  type continuation = (Core.state, Core.state) Unification.continuation 
  
  let trivial_continuation state = Some state
  
  type 'a unifier = (Core.state, Core.state, 'a) Unification.cps_unifier
  
  let realize cont = cont Core.empty_state
  
  type state_check = Core.state Fun.predicate
  
  let mk_assert_check c state =
    let v = (c state) in
    assert (v); v
    
  let mk_verifier check state =
    Option.mk (check state) state
  
  type update_check = (Core.state Unification.state_update) Fun.predicate
  
end

module Unidirectional =
struct
  module Core =
  struct
    type state = Sl_subst.t * Tagpairs.t
    let empty_state = Sl_subst.empty, Tagpairs.empty
  end
  
  include Core
  include Make(Core)

  let test_bindings test combine old_state new_state =
    let rec f ctxt = function
      | [] -> true
      | b::bs -> (test ctxt b) && f (combine b ctxt) bs
    in f old_state new_state
    
  let existential_split_check (trm_subst, tag_subst) =
    Sl_term.Map.for_all
      (fun k v ->
        Sl_term.equal k v ||
        Sl_term.Map.exists
          (fun k' v' -> not (Sl_term.equal k k') && Sl_term.equal v v')
          trm_subst)
      trm_subst
        &&
    Tagpairs.for_all
      (fun (t, t') ->
        Tags.Elt.equal t t' ||
        Tagpairs.exists
          (fun (t'', t''') -> 
            not (Tags.Elt.equal t t'') && Tags.Elt.equal t' t''')
          tag_subst)
      tag_subst
  
  let modulo_entl (_, (trm_subst, tag_subst)) =
    Sl_term.Map.for_all (fun x _ -> Sl_term.is_exist_var x) trm_subst
        &&
    Tagpairs.for_all (fun (t, _) -> Tags.is_exist_var t) tag_subst
  
  let existential_intro 
      ((trm_subst_old, tag_subst_old), (trm_subst_new, tag_subst_new)) =
    let tag_test theta (t, t') =
      Tags.is_exist_var t && Tags.is_free_var t' &&
      Tagpairs.for_all (fun (_, t'') -> not (Tags.Elt.equal t' t'')) theta in
    let trm_test theta (x, y) =
      Sl_term.is_exist_var x && Sl_term.is_free_var y &&
      Sl_term.Map.for_all (fun _ z -> not (Sl_term.equal y z)) theta in
    test_bindings
      tag_test Tagpairs.add tag_subst_old (Tagpairs.to_list tag_subst_new)
        &&
    test_bindings
      trm_test
      (Fun.uncurry Sl_term.Map.add)
      trm_subst_old 
      (Sl_term.Map.bindings trm_subst_new)

  let is_substitution (_, (trm_subst, tag_subst)) =
    Sl_term.Map.for_all
      (fun x y -> 
        Sl_term.is_free_var x && (Sl_term.is_nil y || Sl_term.is_free_var y))
      trm_subst
        &&
    Tagpairs.for_all
      (fun tp -> Pair.both (Pair.map Tags.is_free_var tp))
      tag_subst
  
  let trm_check ((theta, _), (theta', _)) =
    let test sub (x, y) =
      Sl_term.is_free_var x ||
      Sl_term.is_free_var y ||
      Sl_term.is_exist_var x && Sl_term.is_nil y ||
      Sl_term.is_exist_var x && Sl_term.is_exist_var y &&
        Sl_term.Map.for_all (fun _ z -> not (Sl_term.equal y z)) sub in
    test_bindings 
      test (Fun.uncurry Sl_term.Map.add) theta (Sl_term.Map.bindings theta')
    
  let tag_check ((_, theta), (_, theta')) =
    let injective p = Tagpairs.for_all
      (fun p' -> not (Tags.Elt.equal (fst p) (fst p')) || (Tags.Elt.equal (snd p) (snd p'))) in
    let surjective p = Tagpairs.for_all
      (fun p' -> not (Tags.Elt.equal (snd p) (snd p'))) in
    (* Just allow standard substitution and alpha-renaming for now *)
    let test = (Fun.curry 
      (Fun.disj
        (fun (tps, p) -> 
          Pair.conj (Pair.map Tags.is_free_var p) && injective p tps)
        (fun (tps, p) -> Pair.conj (Pair.map Tags.is_exist_var p)
            && injective p tps && surjective p tps))) in
    test_bindings test Tagpairs.add theta (Tagpairs.to_list theta')
  
  let avoid_replacing_trms ?(inverse=false) vars (_, (theta, _)) = 
    Sl_term.Map.for_all
      (Fun.direct inverse 
        (fun x _ -> not (Sl_term.Set.mem x vars)))
      theta
      
  let avoid_replacing_tags ?(inverse=false) tags (_, (_, theta)) = 
    Tagpairs.for_all
      (Fun.uncurry 
        (Fun.direct inverse 
          (fun t _ -> not (Tags.mem t tags))))
      theta
      
  let existentials_only (_, (trm_subst, tag_subst)) =
    Sl_term.Map.for_all
      (fun x y -> 
        Sl_term.equal x y || (Sl_term.is_exist_var x && Sl_term.is_exist_var y))
      trm_subst
        &&
    Tagpairs.for_all
      (fun (t, t') ->
        Tags.Elt.equal t t' || (Tags.is_exist_var t && Tags.is_exist_var t'))
      tag_subst
  
  let unify_tag ?(update_check=Fun._true) t t' cont init_state =
    let extract s = snd s in
    let recombine (theta, _) tps = (theta, tps) in
    let update_check (tps, tps') = 
      let state = recombine init_state tps in
      let state' = (Sl_term.Map.empty, tps') in
      update_check (state, state') in
    Unification.transform extract recombine
      (Tags.Elt.unify ~update_check)
      t t' cont init_state
  
  let unify_tag_constraints 
      ?(total=false) ?(inverse=false) ?(update_check=Fun._true) 
      cs cs' cont init_state =
    let extract s = snd s in
    let recombine (theta, _) tps = (theta, tps) in
    let update_check (tps, tps') = 
      let state = recombine init_state tps in
      let state' = (Sl_term.Map.empty, tps') in
      update_check (state, state') in
    Unification.transform extract recombine
      (Ord_constraints.unify ~total ~inverse ~update_check)
      cs cs' cont init_state
    
  let unify_trm ?(update_check=Fun._true) t t' cont init_state =
    let extract s = fst s in
    let recombine (_, tps) theta = (theta, tps) in
    let update_check (theta, theta') = 
      let state = recombine init_state theta in
      let state' = (theta', Tagpairs.empty) in
      update_check (state, state') in
    Unification.transform extract recombine (Sl_term.unify ~update_check)
      t t' cont init_state
    
  let unify_trm_list ?(update_check=Fun._true) ts ts' cont init_state =
    let extract s = fst s in
    let recombine (_, tps) theta = (theta, tps) in
    let update_check (theta, theta') = 
      let state = recombine init_state theta in
      let state' = (theta', Tagpairs.empty) in
      update_check (state, state') in
    Unification.transform extract recombine (Sl_term.FList.unify ~update_check)
      ts ts' cont init_state

  let remove_dup_substs states =
    let check_add substs ((trm_subst, tag_subst) as subst) =
      let (trm_subst, _) = Sl_subst.partition trm_subst in
      let (tag_subst, _) = Tagpairs.partition_subst tag_subst in
      if Blist.exists
        (fun (trm_subst', tag_subst') -> 
          let (trm_subst', _) = 
            Sl_subst.partition trm_subst' in
          let (tag_subst', _) = 
            Tagpairs.partition_subst tag_subst' in
          Sl_term.Map.equal Sl_term.equal trm_subst trm_subst'
            &&
          Tagpairs.equal tag_subst tag_subst')
        substs
      then substs
      else subst::substs in
    Blist.fold_left check_add Blist.empty states   

end

module Bidirectional =
struct
  module Core =
  struct
    type state = Unidirectional.state * Unidirectional.state
    let empty_state = Unidirectional.empty_state, Unidirectional.empty_state
  end
  
  include Core
  include Make(Core)

  let unify_tag ?(update_check=Fun._true) t t' cont init_state =
    let extract s = Pair.map snd s in
    let recombine ((theta, _), (theta', _)) (tag_subst, tag_subst') = 
      ((theta, tag_subst), (theta', tag_subst')) in
    let update_check (old, update) = 
      let old' = recombine init_state old in
      let update' = Pair.map (Pair.mk Sl_term.Map.empty) update in
      update_check (old', update') in
    Unification.transform extract recombine (Tags.Elt.biunify ~update_check)
      t t' cont init_state

  let unify_tag_constraints 
      ?(total=false) ?(update_check=Fun._true) cs cs' cont init_state =
    let extract s = Pair.map snd s in
    let recombine ((theta, _), (theta', _)) (tag_subst, tag_subst') = 
      ((theta, tag_subst), (theta', tag_subst')) in
    let update_check (old, update) = 
      let old' = recombine init_state old in
      let update' = Pair.map (Pair.mk Sl_term.Map.empty) update in
      update_check (old', update') in
    Unification.transform extract recombine
      (Ord_constraints.biunify ~total ~update_check)
      cs cs' cont init_state
    
  let unify_trm ?(update_check=Fun._true) t t' cont init_state =
    let extract s = Pair.map fst s in
    let recombine ((_, tps), (_, tps')) (theta, theta') = 
      ((theta, tps), (theta', tps')) in
    let update_check (old, update) = 
      let old' = recombine init_state old in
      let update' = Pair.map (Fun.swap Pair.mk Tagpairs.empty) update in
      update_check (old', update') in
    Unification.transform extract recombine (Sl_term.biunify ~update_check)
      t t' cont init_state
    
  let unify_trm_list ?(update_check=Fun._true) ts ts' cont init_state =
    let extract s = Pair.map fst s in
    let recombine ((_, tps), (_, tps')) (theta, theta') = 
      ((theta, tps), (theta', tps')) in
    let update_check (old, update) = 
      let old' = recombine init_state old in
      let update' = Pair.map (Fun.swap Pair.mk Tagpairs.empty) update in
      update_check (old', update') in
    Unification.transform 
      extract recombine (Sl_term.FList.biunify ~update_check)
      ts ts' cont init_state
      
      
  let updchk_inj_left chk state_update =
    chk (Pair.map fst state_update)

  let updchk_inj_right chk state_update =
    chk (Pair.map snd state_update)

end