(** Tagged predicate, as a pair of an integer and a predicate. *)
include Util.BasicType with type t = int * Sld_pred.t

val subst : Sld_term.substitution -> t -> t

val subst_tag : Util.TagPairs.t -> t -> t
(** Substitute the tag according to the function represented by the set of 
    tag pairs provided. *)

val predsym : t -> Sld_predsym.t
val arity : t -> int
val args : t -> Sld_term.t list

val terms : t -> Sld_term.Set.t
val vars : t -> Sld_term.Set.t
val to_string : t -> string
val to_melt : t -> Latex.t
val parse : (t, 'a) MParser.parser

val unify : ?tagpairs:bool -> t Sld_term.unifier
(** Unify two tagged predicates.
    If the optional argument [~tagpairs=false] is set to [true] then also 
    add the pair of tags of the unified predicates. *) 

val norm : Sld_uf.t -> t -> t
(** Replace all terms with their UF representative. NB this may replace [nil] 
    with a variable. *)
 
