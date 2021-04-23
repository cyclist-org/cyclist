(** An ordered pair of SL terms. *)

open Lib

include BasicType with type t = Term.t * Term.t

val unify :
     ?order:bool
  -> ?update_check:Unify.Unidirectional.update_check
  -> t Unify.Unidirectional.unifier
(** Unify two pairs of terms, ignoring the pairs' internal ordering of members.
    The optional argument [~order] has a default of [false].  When it is set
    to [true] then the internal order is honoured. *)

val biunify :
     ?order:bool
  -> ?update_check:Unify.Bidirectional.update_check
  -> t Unify.Bidirectional.unifier

val order : t -> t
(** Return a permutation of the input that obeys the ordering [Term.compare].
E.g. [order (x,y)] will return [(y,x)] is [Term.compare x y > 0]. *)

val subst : Subst.t -> t -> t

val to_string_sep : string -> t -> string

(** A list of term pairs. *)
module FList : sig
  include BasicType with type t = t list

  (* val unify_partial : ?order:bool -> ?inverse:bool -> t Unifier.t *)

  val terms : t -> Term.Set.t
  (** Unify all pairs of the 1st argument with a part of the 2nd.
        - The optional argument [~order:false] indicates whether the internal
          ordering of the pair's members is honoured.
        - The optional argument [~inverse:false] indicates if the substitution
          is applied to the second argument (as opposed to the first). *)
end

(** A set of term pairs *)
module ListSet : OrderedContainer with type elt = t
