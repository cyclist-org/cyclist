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

val empty_subst : substitution
val singleton_subst : t -> t -> substitution
val subst : substitution -> t -> t
val pp_subst : Format.formatter -> substitution -> unit

val strip_subst : substitution -> substitution
(** [strip_subst theta] removes all identity bindings from [theta] *)

val avoid_theta : Set.t -> Set.t -> substitution
(** [avoid_theta vars subvars]
    returns a substitution that takes all variables in [subvars] to new
    variables that are outside [vars U subvars], respecting exist/univ
    quantification.
*)

val mk_univ_subst : Set.t -> Set.t -> substitution
val mk_ex_subst : Set.t -> Set.t -> substitution

val partition_subst : substitution -> (substitution * substitution)
(** [partition_subst theta] will partition [theta] into ([theta_1], [theta_2])
    such that [theta_1] contains all and only the mappings in [theta] from 
    a universally quantified (i.e. free) variable to either [nil] or another
    free variable; that is [theta_1] is the part of [theta] which is a proper
    (proof-theoretic) substitution.
*)

val unify : ?update_check: (substitution * substitution) Fun.predicate -> 
  (substitution, 'a, t) Unification.cps_unifier 
  
val biunify: 
  ?update_check:((substitution * substitution) * (substitution * substitution)) 
    Fun.predicate
      -> (substitution * substitution, 'a, t) Unification.cps_unifier

module FList : 
  sig
    include Util.BasicType with type t = t list
    val unify : 
      ?update_check: (substitution * substitution) Fun.predicate -> 
        (substitution, 'a, t) Unification.cps_unifier
    val biunify: 
      ?update_check:
        ((substitution * substitution) * (substitution * substitution)) 
          Fun.predicate
            -> (substitution * substitution, 'a, t) Unification.cps_unifier
    val subst : substitution -> t -> t
    val to_string_sep : string -> t -> string
    val terms : t -> Set.t
    val vars : t -> Set.t
  end
(** A list of terms. *)

