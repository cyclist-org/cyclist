(** Module defining SL terms, which consist of variables (free
    or existentially quantified), or the constant [nil].
    The focus on fresh variable generation is human readable formulas,
    not speed. *)

include Utilsigs.BasicType

module Set : Utilsigs.OrderedContainer with type elt = t
(** An ordered set of terms. *)

module Map : Utilsigs.OrderedMap with type key = t
(** An ordered map with terms as keys. *)

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


module Subst : VarManager.SubstSig
  with type t = t Map.t
  with type var = t
  with type var_container = Set.t
(** Substitutions over terms *)

val unify : ?update_check: (Subst.t * Subst.t) Fun.predicate ->
  (Subst.t, 'a, t) Unification.cps_unifier
(** Unifies two terms by producing a substitution to act on the first term *)

val biunify:
  ?update_check:((Subst.t * Subst.t) * (Subst.t * Subst.t))
    Fun.predicate
      -> (Subst.t * Subst.t, 'a, t) Unification.cps_unifier
(** Unifies two terms by producing substitutions to act on each term respectively *)

module FList :
  sig
    include Utilsigs.BasicType with type t = t list

    val terms : t -> Set.t
    (** Convenience function converting the list to a set. *)
    val vars : t -> Set.t
    (** Returns the set of all elements of the list that are not nil *)

    val to_string_sep : string -> t -> string
    (** [to_string_sep sep ts] converts [ts] to a string with each element separated by [sep]. *)

    val subst : Subst.t -> t -> t
    (** Applies a substitution to the list *)

    val unify :
      ?update_check: (Subst.t * Subst.t) Fun.predicate ->
        (Subst.t, 'a, t) Unification.cps_unifier
    (** Unifies two lists of terms by producing a substitution to act on the first list *)

    val biunify:
      ?update_check:
        ((Subst.t * Subst.t) * (Subst.t * Subst.t))
          Fun.predicate
            -> (Subst.t * Subst.t, 'a, t) Unification.cps_unifier
    (** Unifies two lists of terms by producing substitutions to act on each list respectively *)

  end
