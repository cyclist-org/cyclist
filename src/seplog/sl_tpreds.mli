(** Multiset of tagged predicates. 
    NB no check is made for the uniqueness of tags within set. 
    However, that uniqueness is required if [tagged_part_unify] is to return
    a set of tag pairs that describes an injective function.
    *)
include Util.OrderedContainer with type elt = Sl_tpred.t

val subst : Sl_term.substitution -> t -> t

val terms : t -> Sl_term.Set.t
val vars : t -> Sl_term.Set.t
val tags : t -> Util.Tags.t

val idents : t -> Util.Strng.MSet.t
(** Return multiset of identifiers present. *)

val to_string_list : t -> string list
val to_melt : t -> Latex.t

val freshen_tags : t -> t -> t

val subsumed : Sl_uf.t -> t -> t -> bool

val unify : t Sl_term.gen_unifier
val part_unify : t Sl_term.gen_unifier

type 'a tagged_unifier = 
  (Sl_term.substitution -> Sl_term.substitution option) -> 
    Sl_term.substitution -> 'a -> 'a -> 
      (Sl_term.substitution * Util.TagPairs.t) option

val tagged_part_unify : t tagged_unifier
(** Part unify a set with another and return in addition to the substitution,
    the set of pairs of tags of predicates unified. *)