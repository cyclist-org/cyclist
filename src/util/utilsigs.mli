(** Signatures for containers and essential types. *)

module type BasicType =
  sig
    type t
    val compare : t -> t -> int
    (** Standard comparator, return <0 if first less than second, 0 if equal, >0 if greater. *)
    val equal : t -> t -> bool
    (** Standard equality predicate. *)
    val hash : t -> int
    (** Standard hash function. *)
    val to_string : t -> string
    (** Convert to string. *)
    val pp : Format.formatter -> t -> unit
    (** Pretty printer. *)
  end
(** Most types for use in containers, maps and other stuff must provide the
    above essential methods. *)

module type OrderedContainer =
  sig
    include Set.S
    val of_list : elt list -> t
    val to_list: t -> elt list
    val endomap : (elt -> elt) -> t -> t
    val map_to : ('b -> 'a -> 'a) -> 'a -> (elt -> 'b) -> t -> 'a
    val map_to_list : (elt -> 'a) -> t -> 'a list
    val weave : (elt -> 'a -> 'a list) -> (elt -> 'a -> 'b) -> ('b list -> 'b) -> t -> 'a -> 'b
    val find : (elt -> bool) -> t -> elt
    val find_opt : (elt -> bool) -> t -> elt option
    val find_map : (elt -> 'a option) -> t -> 'a option
    val union_of_list : t list -> t
    val pp : Format.formatter -> t -> unit
    val to_string : t -> string
    val hash : t -> int
    val subsets : t -> t list
    val fixpoint : (t -> t) -> t -> t
    val del_first : (elt -> bool) -> t -> t
    val all_members_of : t -> t -> bool
    val disjoint : t -> t -> bool
  end
(** A (persistent) ordered container, generalising the standard [Set] container. *)
