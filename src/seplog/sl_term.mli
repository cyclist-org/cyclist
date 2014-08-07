(** Module defining SL terms, which consist of variables (universally *)
(** or existentially quantified), or the constant [nil]. *)
(** NB the ordering [compare] makes existential variables least, then [nil], *)
(** and then universal variables.  *)

include Util.BasicType
module Set : Util.OrderedContainer with type elt = t
module Map : Util.OrderedMap with type key = t

val to_melt : t -> Latex.t
val parse : (t, 'a) MParser.parser

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

type 'a unifier = substitution -> 'a -> 'a -> substitution option 
(** A unifier takes a substitution [theta] and two objects [a] and [b], *) 
(** and extends [theta] to [theta'] so that [a[theta']] is equal to [b], *) 
(** returning [Some theta'], or [None] if its impossible to do so. *)

type 'a gen_unifier = (substitution -> substitution option) -> 'a unifier  
(** A generalised unifier takes a continuation function [f] and acts as a *)
(** unifier.  If it can unify its arguments successfully it should pass the *)
(** resulting substitution to [f] which will check and further extend the *)
(** result.  If [f] returns [None] then the unifier should backtrack and *)
(** search for other substitutions that unify its arguments. *)

val empty_subst : substitution
val singleton_subst : t -> t -> substitution
val subst : substitution -> t -> t

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

