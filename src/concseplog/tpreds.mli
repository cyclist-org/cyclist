(** Multiset of tagged predicates.
    NB no check is made for the uniqueness of tags within set.
    However, that uniqueness is required if [tagged_unify_partial] is to return
    a set of tag pairs that describes an injective function.
    *)
open Lib
open Generic

include OrderedContainer with type elt = Tpred.t

val equal_upto_tags : t -> t -> bool
(** Test whether the two arguments are the equal ignoring tags. *)

val subst : Subst.t -> t -> t

val subst_tags : Tagpairs.t -> t -> t
(** Substitute tags according to the function represented by the set of
    tag pairs provided. *)

val terms : t -> Term.Set.t

val vars : t -> Term.Set.t

val tags : t -> Tags.t

val idents : t -> Predsym.MSet.t
(** Return multiset of identifiers present. *)

val strip_tags : t -> Pred.MSet.t
(** Remove tags. *)

val to_string_list : t -> string list

val freshen_tags : t -> t -> t
(** Rename tags in second argument so that they are disjoint to those in the first. *)

val subsumed_upto_tags : ?total:bool -> Uf.t -> t -> t -> bool
(** Test whether the two arguments are the same modulo the provided equalities.
    NB the comparison ignores tags.
    If the optional argument [~total=true] is set to [false] then
    check if the first multiset is a subset of the second modulo equalities. *)

val subsumed : ?total:bool -> Uf.t -> t -> t -> bool
(** Test whether the two arguments are the same modulo the provided equalities.
    Contrary to [subsumed] this includes tags.
    If the optional argument [~total=true] is set to [false] then
    check if the first multiset is a subset of the second modulo equalities. *)

val unify :
     ?total:bool
  -> ?tagpairs:bool
  -> ?update_check:Unify.Unidirectional.update_check
  -> t Unify.Unidirectional.unifier
(** Compute substitution that makes the two multisets equal up to tags.
- If the optional argument [~total=true] is set to [false] then
  compute substitution that makes the first multiset a subset of the second. *)

val biunify :
     ?total:bool
  -> ?tagpairs:bool
  -> ?update_check:Unify.Bidirectional.update_check
  -> t Unify.Bidirectional.unifier

val norm : Uf.t -> t -> t
(** Replace all terms with their UF representative. NB this may replace [nil]
    with a variable. *)
