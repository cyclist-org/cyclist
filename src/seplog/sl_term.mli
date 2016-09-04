(** Module defining SL terms, which consist of variables (free 
    or existentially quantified), or the constant [nil]. 
    The focus on fresh variable generation is human readable formulas,
    not speed. *)

include Util.BasicType

module Set : Util.OrderedContainer with type elt = t
(** An ordered set of terms. *)

module Map : Util.OrderedMap with type key = t
(** An ordered map with terms as keys. *)

val to_melt : t -> Latex.t
(** Convert term to LaTeX. *)

val parse : (t, 'a) MParser.parser
(** Parse a term. *)

val of_string : string -> t
(** Parse a term from a string. *)

val nil : t
(** The [nil] constant. This is the only non-variable term. *)

val is_nil : t -> bool
(** Is the argument [nil]? Equivalent to [equal nil x]. *)

val is_var : t -> bool
(** [is_nil x] is equivalent to [not (equal nil x)]. *)

val is_exist_var : t -> bool
(** Is the argument an existentially quantified variable? *)

val is_free_var : t -> bool
(** Is the argument a free variable? *)

val filter_vars : Set.t -> Set.t
(** Remove [nil] from a set of terms. *)

val fresh_fvar : Set.t -> t
(** [fresh_fvar s] returns a free variable that is fresh in [s]. *)

val fresh_fvars : Set.t -> int -> t list
(** [fresh_fvars s n] returns a list of free variables of length [n] 
    all of which are fresh in [s]. *)

val fresh_evar : Set.t -> t
(** [fresh_evar s] returns an existentially quantified variable that is 
    fresh in [s]. *)

val fresh_evars : Set.t -> int -> t list
(** [fresh_evars s n] returns a list of existentially quantified variables 
    of length [n] all of which are fresh in [s]. *)

(* to allow reference from within Subst *)
type term_t = t

module type SubstSig =
sig
  type t = term_t Map.t
  (** The type of a substitution is a map from terms to terms. *)
  
  val empty : t
  (** The empty substitution, which has no effect when applied. *)

  val singleton : term_t -> term_t -> t
  (** Constructor for a substitution mapping one variable to a term. *)
  
  val of_list : (term_t * term_t) list -> t
  
  val avoid : Set.t -> Set.t -> t
  (** [avoid vars subvars] 
      returns a substitution that takes all variables in [subvars] to  
      variables fresh in [vars U subvars], respecting existential   
      quantification / free variables. *)
  
  val pp : Format.formatter -> t -> unit
  (** Pretty printer. *)
  
  type check = t -> term_t -> term_t -> bool
  (** The type of functions that check the validity of a single substitution pair
      within the context of the whole substitution to which it is being added. *)
  
  val trivial_check : check
  (** The check that is always true. *)

  val basic_lhs_down_check : check
  (** When used as [subst_check] in the call [Sl_heap.unify subst_check f f'], 
      ensures that the generated substitution, when applied to [f], produces a
      formula which is subsumed by [f'] *)
  val avoids_replacing_check : ?inverse:bool -> Set.t -> check
  (** A substitution check which prevents replacements of variables within the
      given set of terms *)
  val combine_checks : check list -> check
  (** Combinator which takes a list of substitution pair check functions and
      combines them into a single check function *)
end

module Subst : SubstSig
(** A substitution is a map from terms to terms but with some restrictions:
- Only variables can be in the domain of the map.
- An existentially quantified variable can only be mapped to an existential one,
or [nil].
*)

val subst : Subst.t -> t -> t
(** Apply a substitution on the given term. *)


module type UnifierSig =
  sig
    type state = Subst.t * Util.TagPairs.t
    (** State maintained by unifiers. *)
    val empty_state : state
    (** The unifier state consisting of the empty substitution and the empty set of
        tag pairs *)
    val pp_state : Format.formatter -> state -> unit
  
    type continuation = state -> state option 
    val trivial_continuation : continuation
    val basic_lhs_down_verifier : continuation
    (** A continutation generated from the [basic_lhs_down_check] substitution 
        check *)
  
    type 'a t = 
      ?sub_check:Subst.check ->
        ?cont:continuation ->
          ?init_state:state -> 'a -> 'a ->
            state option
  
    val backtrack : 
      'a t -> 
        ?sub_check:Subst.check -> ?cont:continuation -> ?init_state:state -> 
          'a -> 'a -> state list
    (** Wrap a unifier into a function that always backtracks, collecting all 
        states such that [cont state] is not [None] and returning that list. *)
  
    type state_check = state -> bool
    (** The type of functions that check the validity of a whole unifier state *)
  
    val mk_assert_check : state_check -> state_check
    (** Takes a state check and wraps it in an assert *)
  
    val mk_verifier : state_check -> continuation
    (** Takes a state checking function and converts it into a continuation which
        returns None if the check fails *)
  
    val combine_state_checks : state_check list -> state_check
    (** Combinator which takes a list of unifier state check functions and
        combines them into a single check function *)
      
    val lift_subst_check : Subst.check -> state_check
    (** [lift_subst_check c] takes a substitution pair check function [c] and lifts
        it to a unifier state check function by applying it to each entry in the
        state's substitution map *)
  end

module Unifier : UnifierSig

val unify : t Unifier.t 
(** [unify check cont state o o'] should try to extend [state] so that [o] is 
    unified with [o'], extending the variable substitution inside [state]. 
    If this is impossible, then [unify] must return [None].  Otherwise, the 
    new [state'] is to be passed to the continuation function [cont], which
    may carry on with the unification of other objects, validate the state in
    various ways or even record it and return [None] in which case back-tracking
    should occur. The [check] predicate should be applied to each individual 
    term replacement as it is computed, causing the unification to fail if the
    result is false.
    
    Some unifiers may add to the tag pairs in [state], or may not, depending on
    their function. *)



module FList : 
  sig
    include Util.BasicType with type t = t list
    val unify : t Unifier.t
    val subst : Subst.t -> t -> t
    val to_string_sep : string -> t -> string
    val terms : t -> Set.t
    val vars : t -> Set.t
  end
(** A list of terms. *)

