(** Tagged predicate, as a pair of an integer and a predicate. *)
include Utilsigs.BasicType with type t = Tags.Elt.t * Sl_pred.t

val equal_upto_tags : t -> t -> bool
(** Compare for equality two tagged predicates while ignoring tags. *)

val subst : Sl_subst.t -> t -> t

val subst_tag : Tagpairs.t -> t -> t
(** Substitute the tag according to the function represented by the set of
    tag pairs provided. *)

val predsym : t -> Sl_predsym.t
val tag : t -> Tags.Elt.t
val arity : t -> int
val args : t -> Sl_term.t list
val tag : t -> Tags.Elt.t
val tags : t -> Tags.t

val terms : t -> Sl_term.Set.t
val vars : t -> Sl_term.Set.t

val is_tagged : t -> bool
val tag_is_free : t -> bool
val tag_is_exist : t -> bool

val to_string : t -> string
val to_melt : t -> Latex.t
val parse : ?allow_tags:bool -> (t, 'a) MParser.parser
val of_string : string -> t

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
