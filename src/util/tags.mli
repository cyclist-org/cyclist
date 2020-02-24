(** A set of tags. *)

(* type elt = int *)
module Elt : sig
  include Utilsigs.BasicType

  module Unifier : Utilsigs.OrderedContainer with type elt = t * t

  val parse : (t, 'a) MParser.t

  val to_int : t -> int

  val unify :
       ?update_check:Unifier.t Unification.state_update Fun.predicate
    -> (Unifier.t, 'a, t) Unification.cps_unifier
  (** Unifies two tags by finding a suitable substitution (represented as a set of tag pairs)
      for the first tag which makes it equal to the second. *)

  val biunify :
       ?update_check:(Unifier.t * Unifier.t) Unification.state_update
                     Fun.predicate
    -> (Unifier.t * Unifier.t, 'a, t) Unification.cps_unifier
  (** Unifies two tags by finding suitable substitutions (represented as a set of tag pairs)
      for each tag which makes them equal. *)
end

include Utilsigs.OrderedContainer with type elt = Elt.t

include VarManager.I with type var := elt and type var_container = t

val alphabet : VarManager.alphabet ref
(** The alphabet that is used to generate new tags. *)

val anonymous : elt

val to_ints : t -> Int.Set.t
