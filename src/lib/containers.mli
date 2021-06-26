(** Given a [BasicType] build a module with several containers for it. *)

(** The signature of the containers collection. *)
module type S = sig
  module Set : Utilsigs.OrderedContainer

  module Map : Utilsigs.OrderedMap

  module Hashmap : Hashtbl.S

  module Hashset :
    sig
      include Hashset.S
      (* include Utilsigs.OrderedContainer with type t := t *)
      val inter : t -> t -> t
      val exists : (elt -> bool) -> t -> bool
      val for_all : (elt -> bool) -> t -> bool
      val left_union : t -> t -> t
      val is_empty : t -> bool
      val filter : (elt -> bool) -> t -> unit
      val to_string : t -> string
      val of_list : elt list -> t
      val to_list : t -> elt list
      val map_to : ('b -> 'a -> 'a) -> 'a -> (elt -> 'b) -> t -> 'a
      val singleton : elt -> t
    end

  module MSet : Utilsigs.OrderedContainer

  module FList : Utilsigs.BasicType
end

(** Functor putting together the different containers. *)
module Make (T : Utilsigs.BasicType) :
  S
  with type Set.elt = T.t
  with type Map.key = T.t
  with type Hashmap.key = T.t
  with type Hashset.elt = T.t
  with type MSet.elt = T.t
  with type FList.t = T.t list
