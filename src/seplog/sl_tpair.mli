(** An ordered pair of SL terms. *)

include Util.BasicType with type t = Sl_term.t * Sl_term.t

val unify : (t, 'a) Sl_term.unifier

val unord_unify : (t, 'a) Sl_term.unifier
(** Unify two pairs of terms ignoring their internal order.  *)

val order : t -> t
(** Return a permutation of the input that obeys the ordering [Sl_term.compare].
E.g. [order (x,y)] will return [(y,x)] is [Sl_term.compare x y > 0]. *)

val subst : Sl_term.substitution -> t -> t

val to_string_sep : string -> t -> string
val to_melt_sep : Latex.t -> t -> Latex.t

module FList : 
  sig
    include Util.BasicType with type t = t list
    
    val unord_unify_within : (t, 'a) Sl_term.unifier
    (** Unify all (unordered) pairs of the 1st argument with a part of the 2nd. *)

    val inverse_unord_unify_within : (t, 'a) Sl_term.unifier
    (** Like [unord_unify_within] but applying the substitution to the 2nd arg. *)

    val terms : t -> Sl_term.Set.t
  end
(** A list of term pairs. *)
