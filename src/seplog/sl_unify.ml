open Util

type state = Sl_term.substitution * TagPairs.t

let empty_state = Sl_term.empty_subst, TagPairs.empty

type continuation = (state, state) Unification.continuation 

let trivial_continuation state = Some state

type 'a unifier = (state, state, 'a) Unification.cps_unifier

let realize cont = cont empty_state

type state_check = state Fun.predicate

let mk_assert_check c state =
  let v = (c state) in
  assert (v); v

let mk_verifier check state =
  Option.mk (check state) state

type update_check = (state * state) Fun.predicate

let test_bindings test combine old_state new_state =
  let rec f ctxt = function
    | [] -> true
    | b::bs -> (test ctxt b) && f (combine b ctxt) bs
  in f old_state new_state

let trm_check ((theta, _), (theta', _)) =
  let test sub (x, y) =
    Sl_term.is_univ_var x ||
    Sl_term.is_univ_var y ||
    Sl_term.is_exist_var x && Sl_term.is_nil y ||
    Sl_term.is_exist_var x && Sl_term.is_exist_var y &&
      Sl_term.Map.for_all (fun _ z -> not (Sl_term.equal y z)) sub in
  test_bindings test (Fun.uncurry Sl_term.Map.add) 
    theta (Sl_term.Map.bindings theta')
  
let tag_check ((_, theta), (_, theta')) =
  let injective p = TagPairs.for_all
    (fun p' -> not (Tags.Elt.equal (fst p) (fst p')) || (Tags.Elt.equal (snd p) (snd p'))) in
  let surjective p = TagPairs.for_all
    (fun p' -> not (Tags.Elt.equal (snd p) (snd p'))) in
  (* Just allow standard substitution and alpha-renaming for now *)
  let test = (Fun.curry 
    (Fun.disj
      (fun (tps, p) -> 
        Pair.conj (Pair.map Tags.is_univ_var p) && injective p tps)
      (fun (tps, p) -> Pair.conj (Pair.map Tags.is_exist_var p)
          && injective p tps && surjective p tps))) in
  test_bindings test TagPairs.add theta (TagPairs.to_list theta')

let avoid_replacing_trms ?(inverse=false) vars (_, (theta, _)) = 
  Sl_term.Map.for_all
    (Fun.direct inverse 
      (fun x y -> Sl_term.equal x y || not (Sl_term.Set.mem x vars)))
    theta

let unify_tag_constraints 
    ?(inverse=false) ?(update_check=Fun._true) cs cs' cont init_state =
  let extract s = snd s in
  let recombine (theta, _) tps = (theta, tps) in
  let update_check (tps, tps') = 
    let state = recombine init_state tps in
    let state' = (Sl_term.Map.empty, tps') in
    update_check (state, state') in
  let u = Unification.transform extract recombine 
    (Ord_constraints.unify ~inverse ~update_check) in
  u cs cs' cont init_state
  
let unify_trm ?(update_check=Fun._true) cs cs' cont init_state =
  let extract s = fst s in
  let recombine (_, tps) theta = (theta, tps) in
  let update_check (theta, theta') = 
    let state = recombine init_state theta in
    let state' = (theta', TagPairs.empty) in
    update_check (state, state') in
  let u = Unification.transform extract recombine 
    (Sl_term.unify ~update_check) in
  u cs cs' cont init_state
  
let unify_trm_list ?(update_check=Fun._true) cs cs' cont init_state =
  let extract s = fst s in
  let recombine (_, tps) theta = (theta, tps) in
  let update_check (theta, theta') = 
    let state = recombine init_state theta in
    let state' = (theta', TagPairs.empty) in
    update_check (state, state') in
  let u = Unification.transform extract recombine 
    (Sl_term.FList.unify ~update_check) in
  u cs cs' cont init_state