(** An ordered pair of ASL terms. *)
include Util.BasicType with type t = Asl_term.t * Asl_term.t

val unify : ?order:bool -> t Asl_unifier.t
(** Unify two pairs of terms, ignoring the pairs' internal ordering of members.
    The optional argument [~order] has a default of [false].  When it is set 
    to [true] then the internal order is honoured. *)

val order : t -> t
(** Return a permutation of the input that obeys the ordering [Asl_term.compare].
E.g. [order (x,y)] will return [(y,x)] is [Asl_term.compare x y > 0]. *)

val subst : Asl_subst.t -> t -> t

val to_string_sep : string -> t -> string
val to_melt_sep : Latex.t -> t -> Latex.t

module FList : 
  sig
    include Util.BasicType with type t = t list
    
    val unify_partial : ?order:bool -> ?inverse:bool -> t Asl_unifier.t
    (** Unify all pairs of the 1st argument with a part of the 2nd.
        - The optional argument [~order:false] indicates whether the internal 
          ordering of the pair's members is honoured. 
        - The optional argument [~inverse:false] indicates if the substitution
          is applied to the second argument (as opposed to the first). *)

    val terms : t -> Asl_term.Set.t
  end
(** A list of term pairs. *)