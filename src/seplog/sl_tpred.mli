(** Tagged predicate, as a pair of an integer and a predicate. *)
include Util.BasicType with type t = Util.Tags.elt * Sl_pred.t
val subst : Sl_term.substitution -> t -> t

val subst_tag : Util.TagPairs.t -> t -> t
(** Substitute the tag according to the function represented by the set of
    tag pairs provided. *)

val predsym : t -> Sl_predsym.t
val tag : t -> Util.Tags.elt
val arity : t -> int
val args : t -> Sl_term.t list

val terms : t -> Sl_term.Set.t
val vars : t -> Sl_term.Set.t

val is_tagged : t -> bool
val tag_is_univ : t -> bool
val tag_is_exist : t -> bool

val to_string : t -> string
val to_melt : t -> Latex.t
val parse : ?allow_tags:bool -> (t, 'a) MParser.parser

val unify : 
  ?tagpairs:bool -> ?update_check:Sl_unify.Unidirectional.update_check 
    -> t Sl_unify.Unidirectional.unifier
(** Unify two tagged predicates.
    If the optional argument [~tagpairs=false] is set to [true] then also
    add the pair of tags of the unified predicates. *)

val biunify : 
  ?tagpairs:bool -> ?update_check:Sl_unify.Bidirectional.update_check 
    -> t Sl_unify.Bidirectional.unifier

val norm : Sl_uf.t -> t -> t
(** Replace all terms with their UF representative. NB this may replace [nil]
    with a variable. *)
