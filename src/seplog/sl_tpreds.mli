(** Multiset of tagged predicates. 
    NB no check is made for the uniqueness of tags within set. 
    However, that uniqueness is required if [tagged_unify_with_part] is to return
    a set of tag pairs that describes an injective function.
    *)
include Util.OrderedContainer with type elt = Sl_tpred.t

val subst : Sl_term.substitution -> t -> t

val subst_tags : Util.TagPairs.t -> t -> t
(** Substitute tags according to the function represented by the set of 
    tag pairs provided. *)


val terms : t -> Sl_term.Set.t
val vars : t -> Sl_term.Set.t
val tags : t -> Util.Tags.t

val idents : t -> Util.Strng.MSet.t
(** Return multiset of identifiers present. *)

val strip_tags : t -> Sl_pred.MSet.t
(** Remove tags. *)

val to_string_list : t -> string list
val to_melt : t -> Latex.t

val freshen_tags : t -> t -> t

val subsumed : Sl_uf.t -> t -> t -> bool
(** Test whether the two arguments are the same modulo the provided equalities. 
    NB the comparison ignores tags. *) 
val tagged_subsumed : Sl_uf.t -> t -> t -> bool
(** Test whether the two arguments are the same modulo the provided equalities. 
    Contrary to [subsumed] this includes tags. *) 

val unify : t Sl_term.gen_unifier
val unify_with_part : t Sl_term.gen_unifier

val tagged_unify : t Sl_term.tagged_unifier
(** Part unify a set with another and return in addition to the substitution,
    the set of pairs of tags of predicates unified. *)