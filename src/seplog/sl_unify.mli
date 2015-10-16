(** This module specialises the generic Unification module to the SL 
    instantiation of cyclist and provides extra functionality to support the 
    unification of its various elements
*)

type state = Sl_term.substitution * Util.TagPairs.t
(** State maintained by unifiers. *)

val empty_state : state
(** The unifier state consisting of the empty substitution and the empty set of
    tag pairs *)
    
type continuation = (state, state) Unification.continuation 
(** The type of continuations accepted by SL cps-unifiers *)

type 'a unifier = (state, state, 'a) Unification.cps_unifier
(** The type of SL unifiers - these always act on pairs of term substitutions
    and tagpair sets, and only accept continuations that are maps on such pairs.
*)

val realize : (state, 'a) Unification.realizer

type state_check = state Fun.predicate
(** Predicates that check unifier states for validity *)

val mk_assert_check : state_check -> state_check
(** Takes a state check and wraps it in an assert *)

val mk_verifier : state_check -> continuation
(** Takes a state check function and converts it into a continuation which
    returns None if the check fails *)

type update_check = (state * state) Fun.predicate

val trm_check : update_check
(** When used as state update check in a call to [Sl_heap.unify] for unifying
    heaps [h] and [h'], ensures that the generated substitution, when applied 
    to [h], produces a formula which is subsumed by [h'] *)
  
val avoid_replacing_trms : ?inverse:bool -> Sl_term.Set.t -> update_check
(** A state update check which prevents replacements of variables within the
    given set of terms. When the optional flag [inverse=false] is set to true
    the check prevents replacements from substituting any of the variables
    within the given set *)
    
val tag_check : update_check

val unify_tag_constraints : 
  ?inverse:bool -> ?update_check:update_check -> Ord_constraints.t unifier
(** [Ord_constraints.unify] lifted to the SL unifier type *)

val unify_trm : ?update_check:update_check -> Sl_term.t unifier

val unify_trm_list : ?update_check:update_check -> Sl_term.FList.t unifier

  