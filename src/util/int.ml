module IntType : Utilsigs.BasicType with type t = int =
  struct
    type t = int
    let compare (i:t) (j:t) = if i<j then -1 else if i>j then +1 else 0
    let equal (i:t) (j:t) = i=j
    let hash (i:t) = Hashtbl.hash i
    let to_string = string_of_int
    let pp = Format.pp_print_int
  end

include IntType
include Containers.Make(IntType)

let min (i:int) (j:int) = Pervasives.min i j
let max (i:int) (j:int) = Pervasives.max i j
let (<) (i:int) (j:int) = Pervasives.(<) i j
let (<=) (i:int) (j:int) = Pervasives.(<=) i j
let (>) (i:int) (j:int) = Pervasives.(>) i j
let (>=) (i:int) (j:int) = Pervasives.(>=) i j
let (<>) (i:int) (j:int) = Pervasives.(<>) i j
let (=) i j = equal i j
