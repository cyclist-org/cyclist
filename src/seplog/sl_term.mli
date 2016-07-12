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

type substitution = t Map.t
(** A substitution is a map from terms to terms but with some restrictions:
- Only variables can be in the domain of the map.
- An existentially quantified variable can only be mapped to an existential one,
or [nil].
*)

type unifier_state = substitution * Util.TagPairs.t
(** State maintained by unifiers. *)

val empty_state : unifier_state
(** The unifier state consisting of the empty substitution and the empty set of
    tag pairs *)

type subst_check = substitution -> t -> t -> bool
(** The type of functions that check the validity of a single substitution pair
    within the context of the whole substitution to which it is being added. *)
    
val trivial_sub_check : subst_check

type continuation = unifier_state -> unifier_state option 

val trivial_continuation : continuation

type 'a unifier = 
  ?sub_check:subst_check ->
    ?cont:continuation ->
      ?init_state:unifier_state -> 'a -> 'a ->
        unifier_state option
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

val backtrack : 
  'a unifier -> 
    ?sub_check:subst_check -> ?cont:continuation -> ?init_state:unifier_state -> 
      'a -> 'a -> unifier_state list
(** Wrap a unifier into a function that always backtracks, collecting all 
    states such that [cont state] is not [None] and returning that list. *)

val empty_subst : substitution
val singleton_subst : t -> t -> substitution
val subst : substitution -> t -> t
val pp_subst : Format.formatter -> substitution -> unit

val unify : t unifier 

val avoid_theta : Set.t -> Set.t -> substitution
(** [avoid_theta vars subvars] 
    returns a substitution that takes all variables in [subvars] to new 
    variables that are outside [vars U subvars], respecting existential   
    quantification / free variables. *)

type state_check = unifier_state -> bool
(** The type of functions that check the validity of a whole unifier state *)

val combine_subst_checks : subst_check list -> subst_check
(** Combinator which takes a list of substitution pair check functions and
    combines them into a single check function *)
    
(* val combine_state_checks : state_check list -> state_check *)
(* Combinator which takes a list of unifier state check functions and
    combines them into a single check function *)
    
(* val lift_subst_check : subst_check -> state_check *)
(* [lift_subst_check c] takes a substitution pair check function [c] and lifts 
    it to a unifier state check function by applying it to each entry in the 
    state's substitution map *)
    
val mk_assert_check : state_check -> state_check
(** Takes a state check and wraps it in an assert *)

val mk_verifier : state_check -> continuation
(** Takes a state checking function and converts it into a continuation which
    returns None if the check fails *)

val basic_lhs_down_check : subst_check
(** When used as [subst_check] in the call [Sl_heap.unify subst_check f f'], 
    ensures that the generated substitution, when applied to [f], produces a
    formula which is subsumed by [f'] *)

(* val basic_lhs_down_verifier : continuation *)
(* A continutation generated from the [basic_lhs_down_check] substitution 
    check *)

val avoids_replacing_check : ?inverse:bool -> Set.t -> subst_check
(** A substiution check which prevents replacements of variables within the
    given set of terms *)

module FList : 
  sig
    include Util.BasicType with type t = t list
    val unify : t unifier
    val subst : substitution -> t -> t
    val to_string_sep : string -> t -> string
    val terms : t -> Set.t
    val vars : t -> Set.t
  end
(** A list of terms. *)

