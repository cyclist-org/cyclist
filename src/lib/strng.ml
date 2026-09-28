module StringType : Utilsigs.BasicType with type t = string = struct
  type t = string [@@deriving compare, equal]

  let hash (i : t) = Hashtbl.hash i
  let to_string (i : t) = i
  let pp = Format.pp_print_string
end

include StringType
include Containers.Make (StringType)
