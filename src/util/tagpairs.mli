(** A set of tag pairs with a few convenience functions. Tag pairs are 
    used mainly for representing a transition relation over the set of tags. *)

type elt = int * int

include Utilsigs.OrderedContainer with type elt := elt

val mk : Tags.t -> t
(** [mk tags] computes the identity relation over the set [tags]. *)
val compose : t -> t -> t
(** [compose l r] computes the relation representing the composition of [l] with [r]. *)
val projectl : t -> Tags.t
(** [projectl tps] computes the set of tags appearing in the left of pairs in [tps]. *)
val projectr : t -> Tags.t
(** [projectr tps] computes the set of tags appearing in the right of pairs in [tps]. *)
val reflect : t -> t
(** Reverse the relation. *)
