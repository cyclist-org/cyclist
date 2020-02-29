(** A set of tag pairs with a few convenience functions. Tag pairs are 
    used mainly for representing a transition relation over the set of tags. *)

open Lib

type elt = Tags.Elt.t * Tags.Elt.t

include
  OrderedContainer with type t = Tags.Elt.Unifier.t
                    and type elt := elt

val mk : Tags.t -> t
(** [mk tags] computes the identity relation over the set [tags]. *)

val dest_singleton : t -> elt option
(** [dest_singleton tps] returns [Some(tp)] if [tp] is the only element of [tps], and [None] otherwise. *)

val compose : t -> t -> t
(** [compose l r] computes the relation representing the composition of [l] with [r]. *)

val projectl : t -> Tags.t
(** [projectl tps] computes the set of tags appearing in the left of pairs in [tps]. *)

val projectr : t -> Tags.t
(** [projectr tps] computes the set of tags appearing in the right of pairs in [tps]. *)

val reflect : t -> t
(** Reverse the relation. *)

val apply_to_tag : t -> Tags.Elt.t -> Tags.Elt.t
(** [apply_to_tag tps t] treats [tps] as a substitution and applies it to [t]. *)

val strip : t -> t
(** [strip tps] removes all elements that are pairs of equal tags. *)

val flatten : t -> Tags.t
(** Produce a set of all the tags in the given set of tag pairs. *)

val partition_subst : t -> t * t
(** Partition the set of tag pairs into those pairs containing only "free" tags, and all the rest *)

val mk_free_subst : Tags.t -> Tags.t -> t
(** [mk_free_subst avoid ts] produces a set of tag pairs representing a substitution of pairwise distinct
    free tags (fresh for all the tags in [avoid]) for the tags in [ts]. *)

val mk_ex_subst : Tags.t -> Tags.t -> t
(** [mk_ex_subst avoid ts] produces a set of tag pairs representing a substitution of pairwise distinct
    existentially quantified tags (fresh for all the tags in [avoid]) for the tags in [ts]. *)
