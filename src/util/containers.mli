(** Given a [BasicType] build a module with several containers for it. *)

module type S =
  sig
    module Set : Utilsigs.OrderedContainer
    module Map : Utilsigs.OrderedMap
    module Hashmap : Hashtbl.S
    module Hashset : Hashset.S
    module MSet : Utilsigs.OrderedContainer
    module FList : Utilsigs.BasicType
  end
(** The signature of the containers collection. *)

module Make(T: Utilsigs.BasicType) : 
S with type Set.elt=T.t
  with type Map.key=T.t
  with type Hashmap.key=T.t
  with type Hashset.elt=T.t
  with type MSet.elt=T.t
  with type FList.t=T.t list
(** Functor putting together the different containers. *)