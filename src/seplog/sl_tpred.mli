include Util.BasicType with type t = int * Sl_pred.t

val subst : Sl_term.substitution -> t -> t

val subst_tag : Util.TagPairs.t -> t -> t
(** Substitute the tag according to the function represented by the set of 
    tag pairs provided. *)

val terms : t -> Sl_term.Set.t
val vars : t -> Sl_term.Set.t
val to_string : t -> string
val to_melt : t -> Latex.t
val parse : (t, 'a) MParser.parser
val unify : t Sl_term.unifier

val tagged_unify : 
  Sl_term.substitution -> t -> t -> (Sl_term.substitution * (int * int)) option
(** Unify two tagged predicates but also return the pair of tags of the 
    unified predicates. *) 