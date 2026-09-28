module IntType : Utilsigs.BasicType with type t = int = struct
  type t = int [@@deriving compare, equal]

  let hash (i : t) = Hashtbl.hash i
  let to_string = string_of_int
  let pp = Format.pp_print_int
end

include IntType
include Containers.Make (IntType)

let min (i : int) (j : int) = Stdlib.min i j
let max (i : int) (j : int) = Stdlib.max i j
let ( < ) (i : int) (j : int) = Stdlib.( < ) i j
let ( <= ) (i : int) (j : int) = Stdlib.( <= ) i j
let ( > ) (i : int) (j : int) = Stdlib.( > ) i j
let ( >= ) (i : int) (j : int) = Stdlib.( >= ) i j
let ( <> ) (i : int) (j : int) = Stdlib.( <> ) i j
let ( = ) i j = equal i j
