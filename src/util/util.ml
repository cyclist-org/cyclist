open Lib

(* boost::hash_combine *)
(* size_t hash_combine( size_t lhs, size_t rhs ) {     *)
(*   lhs^= rhs + 0x9e3779b9 + (lhs << 6) + (lhs >> 2); *)
(*   return lhs;                                       *)
(* }                                                   *)

module type BasicType = Utilsigs.BasicType
module type OrderedContainer = Utilsigs.OrderedContainer
module type OrderedMap = Utilsigs.OrderedMap

module type CTsig =
  sig
    
    module Set : OrderedContainer
    module Map : OrderedMap
    module Hashmap : Hashtbl.S
    module Hashset : Hashset.S
    module MSet : OrderedContainer
    module FList : BasicType
  end

module ContaineriseType(T: BasicType) : CTsig
  with type Set.elt=T.t
  with type Map.key=T.t
  with type Hashmap.key=T.t
  with type Hashset.elt=T.t
  with type MSet.elt=T.t
  with type FList.t=T.t list
  =
  struct
    (* module Set = Listset.Make(T) *)
    module Set = Treeset.Make(T)
    module Map = Treemap.Make(T)
    module Hashmap = Hashtbl.Make(T)
    module Hashset = Hashset.Make(T)
    module MSet = Multiset.Make(T)
    module FList = Flist.Make(T)
  end

module type MCTsig =
  sig
    include BasicType
    module T : BasicType
    include CTsig
  end

module MakeComplexType(T: BasicType) : MCTsig
  with type t=T.t
  with type T.t=T.t
  with type Set.elt=T.t
  with type Map.key=T.t
  with type Hashmap.key=T.t
  with type Hashset.elt=T.t
  with type MSet.elt=T.t
  with type FList.t=T.t list
  =
  struct
    include T
    module T = T
    module Set = Treeset.Make(T)
    module Map = Treemap.Make(T)
    module Hashmap = Hashtbl.Make(T)
    module Hashset = Hashset.Make(T)
    module MSet = Multiset.Make(T)
    module FList = Flist.Make(T)
  end

module IntType : BasicType with type t = int =
  struct
    type t = int
    let compare (i:t) (j:t) = if i<j then -1 else if i>j then +1 else 0
    let equal (i:t) (j:t) = i=j
    let hash (i:t) = Hashtbl.hash i
    let to_string = string_of_int
    let pp = Format.pp_print_int
  end

module StringType : BasicType with type t = string =
  struct
    type t = string
    let compare (i:t) (j:t) = String.compare i j
    let equal (i:t) (j:t) = i=j
    let hash (i:t) = Hashtbl.hash i
    let to_string (i:t) = i
    let pp = Format.pp_print_string
  end

module Int = MakeComplexType(IntType)
module Strng = MakeComplexType(StringType)

module Tags = Int.Set

module TagPairs =
  struct
    module Pairing = MakeComplexType(Pair.Make(Int)(Int))
    include Pairing.Set
    let mk s = Tags.fold (fun i p -> add (i,i) p) s empty

    let compose t1 t2 : t =
      fold
        (fun (x,y) a ->
          fold (fun (w,z) b -> if y=w then add (x,z) b else b) t2 a)
        t1 empty

    let projectl tp = map_to Tags.add Tags.empty fst tp
    let projectr tp = map_to Tags.add Tags.empty snd tp

    let reflect tps = map_to add empty Pair.swap tps
  end


