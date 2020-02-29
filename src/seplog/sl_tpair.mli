(** An ordered pair of SL terms. *)

open Lib

include BasicType with type t = Sl_term.t * Sl_term.t

val unify :
     ?order:bool
  -> ?update_check:Sl_unify.Unidirectional.update_check
  -> t Sl_unify.Unidirectional.unifier
(** Unify two pairs of terms, ignoring the pairs' internal ordering of members.
    The optional argument [~order] has a default of [false].  When it is set
    to [true] then the internal order is honoured. *)

val biunify :
     ?order:bool
  -> ?update_check:Sl_unify.Bidirectional.update_check
  -> t Sl_unify.Bidirectional.unifier

val order : t -> t
(** Return a permutation of the input that obeys the ordering [Sl_term.compare].
E.g. [order (x,y)] will return [(y,x)] is [Sl_term.compare x y > 0]. *)

val subst : Sl_subst.t -> t -> t

val to_string_sep : string -> t -> string

(** A list of term pairs. *)
module FList : sig
  include BasicType with type t = t list

  (* val unify_partial : ?order:bool -> ?inverse:bool -> t Sl_unifier.t *)

  val terms : t -> Sl_term.Set.t
  (** Unify all pairs of the 1st argument with a part of the 2nd.
        - The optional argument [~order:false] indicates whether the internal
          ordering of the pair's members is honoured.
        - The optional argument [~inverse:false] indicates if the substitution
          is applied to the second argument (as opposed to the first). *)
end

(** A set of term pairs *)
module ListSet : OrderedContainer with type elt = t
