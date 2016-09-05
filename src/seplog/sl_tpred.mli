(** Tagged predicate, as a pair of an integer and a predicate. *)
include Util.BasicType with type t = int * Sl_pred.t

val equal_upto_tags : t -> t -> bool
(** Compare for equality two tagged predicates while ignoring tags. *)

val subst : Sl_subst.t -> t -> t

val subst_tag : Util.TagPairs.t -> t -> t
(** Substitute the tag according to the function represented by the set of 
    tag pairs provided. *)

val predsym : t -> Sl_predsym.t
val arity : t -> int
val args : t -> Sl_term.t list
val tag : t -> Util.Tags.elt
val tags : t -> Util.Tags.t

val terms : t -> Sl_term.Set.t
val vars : t -> Sl_term.Set.t

val to_string : t -> string
val to_melt : t -> Latex.t

val parse : (t, 'a) MParser.parser
val of_string : string -> t

val unify : ?tagpairs:bool -> t Sl_unifier.t
(** Unify two tagged predicates.
    If the optional argument [~tagpairs=false] is set to [true] then also 
    add the pair of tags of the unified predicates. *) 

val norm : Sl_uf.t -> t -> t
(** Replace all terms with their UF representative. NB this may replace [nil] 
    with a variable. *)
 