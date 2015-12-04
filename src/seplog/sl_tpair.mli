(** An ordered pair of SL terms. *)

include Util.BasicType with type t = Sl_term.t * Sl_term.t

val unify : 
  ?order:bool -> ?update_check:Sl_unify.Unidirectional.update_check 
    -> t Sl_unify.Unidirectional.unifier
(** Unify two pairs of terms, ignoring the pairs' internal ordering of members.
    The optional argument [~order] has a default of [false].  When it is set 
    to [true] then the internal order is honoured. *)

val biunify : 
  ?order:bool -> ?update_check:Sl_unify.Bidirectional.update_check 
    -> t Sl_unify.Bidirectional.unifier

val order : t -> t
(** Return a permutation of the input that obeys the ordering [Sl_term.compare].
E.g. [order (x,y)] will return [(y,x)] is [Sl_term.compare x y > 0]. *)

val subst : Sl_term.substitution -> t -> t

val to_string_sep : string -> t -> string
val to_melt_sep : Latex.t -> t -> Latex.t

module FList : 
  sig
    include Util.BasicType with type t = t list
    
    val terms : t -> Sl_term.Set.t
  end
(** A list of term pairs. *)

module ListSet : Util.OrderedContainer with type elt = t
(** A set of term pairs *)
