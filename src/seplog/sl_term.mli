(** Module defining SL terms, which consist of variables (universally *)
(** or existentially quantified), or the constant [nil]. *)
(** NB the ordering [compare] makes existential variables least, then [nil], *)
(** and then universal variables.  *)

include Util.BasicType
module Set : Util.OrderedContainer with type elt = t
module Map : Util.OrderedMap with type key = t

val to_melt : t -> Latex.t
val parse : (t, 'a) MParser.parser
val of_string : string -> t

val nil : t

val is_nil : t -> bool
val is_var : t -> bool
val is_exist_var : t -> bool
val is_univ_var : t -> bool

val mk_exist_var : string -> t
val mk_univ_var : string -> t
val filter_vars : Set.t -> Set.t

(** The following functions behave exactly as their namesakes in Var, *) 
(** but for SL. *)

val fresh_uvar : Set.t -> t
val fresh_uvars : Set.t -> int -> t list
val fresh_evar : Set.t -> t
val fresh_evars : Set.t -> int -> t list

type substitution = t Map.t
(** A substitution is a map from terms to terms but with some restrictions:
- Only variables can be in the domain of the map.
- An existentially quantified variable can only be mapped to an existential one,
or [nil].
*)

type unifier_state = substitution * Util.TagPairs.t
(** State maintained by unifiers. *)

type 'a unifier = 
  (unifier_state -> unifier_state option) ->
    unifier_state -> 'a -> 'a ->
      unifier_state option
(** [unify cont state o o'] should try to extend [state] so that [o] is unified
    with [o'], extending the variable substitution inside [state]. 
    If this is impossible, then [unify] must return [None].  Otherwise, the 
    new [state'] is to be passed to the continuation function [cont], which
    may carry on with the unification of other objects, validate the state in
    various ways or even record it and return [None] in which case back-tracking
    should occur. 
    
    Some unifiers may add to the tag pairs in [state], or may not, depending on
    their function. *)

val empty_subst : substitution
val singleton_subst : t -> t -> substitution
val subst : substitution -> t -> t
val pp_subst : Format.formatter -> substitution -> unit

val unify : t unifier 

val avoid_theta : Set.t -> Set.t -> substitution
(** [avoid_theta vars subvars] *)
(** returns a substitution that takes all variables in [subvars] to new *)
(** variables that are outside [vars U subvars], respecting exist/univ   *)
(** quantification. *)

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

