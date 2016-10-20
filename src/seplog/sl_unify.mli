(** This module specialises the generic Unification module to the SL 
    instantiation of cyclist and provides extra functionality to support the 
    unification of its various elements
*)

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
  
  type update_check = (state Unification.state_update) Fun.predicate
  
  val unify_tag : ?update_check:update_check -> Tags.Elt.t unifier
  val unify_trm : ?update_check:update_check -> Sl_term.t unifier
  val unify_trm_list : ?update_check:update_check -> Sl_term.FList.t unifier
end

module Unidirectional : 
sig
  include S with type state = Sl_subst.t * Tagpairs.t

  val existential_split_check : state_check
  
  val modulo_entl : update_check
  val existential_intro : update_check
  val is_substitution : update_check

  val trm_check : update_check
  (** When used as state update check in a call to [Sl_heap.unify] for unifying
      heaps [h] and [h'], ensures that the generated substitution, when applied 
      to [h], produces a formula which is subsumed by [h'] *)
    
  val avoid_replacing_trms : ?inverse:bool -> Sl_term.Set.t -> update_check
  (** A state update check which prevents replacements of variables within the
      given set of terms. When the optional flag [inverse=false] is set to true
      the check prevents replacements from substituting any of the variables
      within the given set *)
      
  val avoid_replacing_tags : ?inverse:bool -> Tags.t -> update_check
  
  val tag_check : update_check
  
  val existentials_only : update_check
  
  val unify_tag_constraints : 
    ?total:bool -> ?inverse:bool -> ?update_check:update_check 
      -> Ord_constraints.t unifier
  (** [Ord_constraints.unify] lifted to the SL unifier type *)
  
  val remove_dup_substs : state list -> state list
  (** [remove_dup_substs states] will remove any states in [states] where the 
      universal parts (i.e. the mappings from universals to universals) of both 
      the tag and term substitutions are duplicates of a previous state. *)
  
end

module Bidirectional :
sig
  include S with type state = Unidirectional.state * Unidirectional.state
  
  val updchk_inj_left : Unidirectional.update_check -> update_check
  val updchk_inj_right : Unidirectional.update_check -> update_check
  
  val unify_tag_constraints : 
    ?total:bool -> ?update_check:update_check -> Ord_constraints.t unifier
  (** [Ord_constraints.biunify] lifted to the SL unifier type *)
end
