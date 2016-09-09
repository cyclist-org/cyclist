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
