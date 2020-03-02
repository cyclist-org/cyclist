open Lib
open Generic

include BasicType

include Containers.S
  with type Set.elt = t
  with type Map.key = t
  with type Hashmap.key = t
  with type Hashset.elt = t
  with type MSet.elt = t
  with type FList.t = t list

val is_eq : t -> bool
val is_deq : t -> bool
val is_ipred : t -> bool

val mk_eq : Term.t -> Term.t -> t
val mk_deq : Term.t -> Term.t -> t
val mk_ipred : Tags.Elt.t -> string -> Term.t list -> t

val dest_eq : t -> Term.t * Term.t
val dest_deq : t -> Term.t * Term.t
val dest_pred : t -> Tags.Elt.t * string * Term.t list

val equal_upto_tags  : t -> t -> bool

val ipred_eq_mod_tags : t -> t -> bool
val vars : t -> Term.Set.t
val terms : t -> Term.Set.t
val tag : t -> Tags.Elt.t option

val repl_tags : Tags.Elt.t -> t -> t
val strip_tags : t -> string * Term.t list

val subst : Term.substitution -> t -> t

val unify_eqs : Term.substitution -> t -> t -> Term.substitution list
val unify_deqs : Term.substitution -> t -> t -> Term.substitution list
val unify_ipreds : Term.substitution -> t -> t -> Term.substitution option

val parse : (t, 'a) MParser.t