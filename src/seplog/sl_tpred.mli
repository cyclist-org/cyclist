(** Tagged predicate, as a pair of an integer and a predicate. *)
include Util.BasicType with type t = int * Sl_pred.t

val subst : Sl_term.substitution -> t -> t

val subst_tag : Util.TagPairs.t -> t -> t
(** Substitute the tag according to the function represented by the set of 
    tag pairs provided. *)

val predsym : t -> Sl_pred.ident_t
val arity : t -> int
val args : t -> Sl_term.t list

val terms : t -> Sl_term.Set.t
val vars : t -> Sl_term.Set.t
val to_string : t -> string
val to_melt : t -> Latex.t
val parse : (t, 'a) MParser.parser

val unify : ?tagpairs:bool -> t Sl_term.unifier
(** Unify two tagged predicates.
    If the optional argument [~tagpairs=false] is set to [true] then also 
    add the pair of tags of the unified predicates. *) 
