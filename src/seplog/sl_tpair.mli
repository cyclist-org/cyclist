(** An ordered pair of SL terms. *)

include Util.BasicType with type t = Sl_term.t * Sl_term.t

val unify : t Sl_term.unifier

val unord_unify : t Sl_term.gen_unifier
(** Unify two pairs of terms ignoring their internal order.  As multiple *)
(** ways to unify may exist, a continuation function has to be provided. *)

val norm : t -> t
(** Return a permutation of the input that obeys the ordering Sl_term.compare. *)

val to_string_sep : string -> t -> string
val to_melt_sep : Latex.t -> t -> Latex.t

module FList : 
  sig
    include Util.BasicType with type t = t list
    
    val part_unify : t Sl_term.gen_unifier
    (** Unify all (unordered) pairs of the 1st argument with *)
    (** a part of the 2nd.  Multiple unifications possible in general, thus *)
    (** a general unifier. *)
  end
(** A list of term pairs. *)
