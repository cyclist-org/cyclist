open Lib

module StringType : Utilsigs.BasicType with type t = string =
  struct
    type t = string
    let compare (i:t) (j:t) = String.compare i j
    let equal (i:t) (j:t) = String.equal i j
    let hash (i:t) = Hashtbl.hash i
    let to_string (i:t) = i
    let pp = Format.pp_print_string
  end

include StringType
include Containers.Make(StringType)
