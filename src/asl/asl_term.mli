(** Module defining ASL terms, which consist of variables (free 
    or existentially quantified), constant natural numbers, addition of terms
    or multiplication between a contant and a term. *)

include Util.BasicType

module Set : Util.OrderedContainer with type elt = t
(** An ordered set of terms. *)

module Map : Util.OrderedMap with type key = t
(** An ordered map with terms as keys. *)

type term_t = t
module type AslSubstSig =
	sig
	  type t = term_t Map.t
	  (** The type of a substitution is a map from terms to terms. *)
	  
	  val empty : t
	  (** The empty substitution, which has no effect when applied. *)

	  val singleton : term_t -> term_t -> t
	  (** Constructor for a substitution mapping one variable to a term. *)
	  
	  val pp : Format.formatter -> t -> unit
	  (** Pretty printer. *)
	  
	  type check = t -> term_t -> term_t -> bool
	  (** The type of functions that check the validity of a single substitution pair
	      within the context of the whole substitution to which it is being added. *)
	  
	  val trivial_check : check
	  (** The check that is always true. *)

	  val basic_lhs_down_check : check
	  (** When used as [subst_check] in the call [Asl_heap.unify subst_check f f'], 
	      ensures that the generated substitution, when applied to [f], produces a
	      formula which is subsumed by [f'] *)
	  val avoids_replacing_check : ?inverse:bool -> Set.t -> check
	  (** A substitution check which prevents replacements of variables within the
	      given set of terms *)
	  val combine_checks : check list -> check
	  (** Combinator which takes a list of substitution pair check functions and
	      combines them into a single check function *)
	end

module Asl_subst : AslSubstSig

module type AslUnifierSig =
  sig
    type state = Asl_subst.t * Util.TagPairs.t
    (** State maintained by unifiers. *)
    val empty_state : state
    (** The unifier state consisting of the empty substitution and the empty set of
        tag pairs *)
  
    type continuation = state -> state option 
    val trivial_continuation : continuation
    val basic_lhs_down_verifier : continuation
    (** A continutation generated from the [basic_lhs_down_check] substitution 
        check *)
  
    type 'a t = 
      ?sub_check:Asl_subst.check ->
        ?cont:continuation ->
          ?init_state:state -> 'a -> 'a ->
            state option
  
    val backtrack : 
      'a t -> 
        ?sub_check:Asl_subst.check -> ?cont:continuation -> ?init_state:state -> 
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
      
    val lift_subst_check : Asl_subst.check -> state_check
    (** [lift_subst_check c] takes a substitution pair check function [c] and lifts
        it to a unifier state check function by applying it to each entry in the
        state's substitution map *)
  end

module Asl_unifier : AslUnifierSig

type term =
| Var of Sl_term.t
| Const of int
| Add of (term * term)
| Mult of (int * term)

val to_melt : t -> Latex.t
(** Convert term to LaTeX. *)

val to_z3 : t -> string
(** Convert term to z3 formatted string. *)

val parse : (t, 'a) MParser.parser
(** Parse a term. *)

val mk_add : t * t -> t
val mk_mult : int * t -> t
val mk_const : int -> t
(** Constructors *)

val of_string : string -> t
(** Parse a term from a string. *)

val filter_vars : Set.t -> Set.t
(** Given a Set of terms returns the Set of all Vars contained in the terms. *)

val subst : Asl_subst.t -> t -> t
(** [subst theta t] returns t with variables substitued according to [theta]. *)

val unify : t Asl_unifier.t 
(** [unify check cont state o o'] should try to extend [state] so that [o] is 
    unified with [o'], extending the variable substitution inside [state]. 
    If this is impossible, then [unify] must return [None].  Otherwise, the 
    new [state'] is to be passed to the continuation function [cont], which
    may carry on with the unification of other objects, validate the state in
    various ways or even record it and return [None] in which case back-tracking
    should occur. The [check] predicate should be applied to each individual 
    term replacement as it is computed, causing the unification to fail if the
    result is false. *)

val simplify : t -> t
(** Simplify term according to simple arithmetic equalities for Addition and Multiplication.*)

val is_free_var : t -> bool
val is_exist_var : t -> bool

val fresh_fvar : Set.t -> t
val fresh_fvars : Set.t -> int -> t list
val fresh_evar : Set.t -> t
val fresh_evars : Set.t -> int -> t list