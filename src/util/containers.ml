module type S =
  sig
    module Set : Utilsigs.OrderedContainer
    module Map : Utilsigs.OrderedMap
    module Hashmap : Hashtbl.S
    module Hashset : Hashset.S
    module MSet : Utilsigs.OrderedContainer
    module FList : Utilsigs.BasicType
  end

module Make(T: Utilsigs.BasicType) =
  struct
    (* module Set = Listset.Make(T) *)
    module Set = Treeset.Make(T)
    module Map = Treemap.Make(T)
    module Hashmap = Hashtbl.Make(T)
    module Hashset = Hashset.Make(T)
    module MSet = Multiset.Make(T)
    module FList = Flist.Make(T)
  end
