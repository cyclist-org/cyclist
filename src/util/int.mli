(** Int [BasicType] with assorted containers. *)

type t = int 

include Utilsigs.BasicType with type t:=t

include Containers.S  
  with type Set.elt=t
  with type Map.key=t
  with type Hashmap.key=t
  with type Hashset.elt=t
  with type MSet.elt=t
  with type FList.t=t list
