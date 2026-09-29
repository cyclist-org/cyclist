open Misc

module Make (T : Utilsigs.BasicType) :
  Utilsigs.BasicType with type t = T.t list = struct
  type t = T.t list [@@deriving compare, equal, hash]

  let pp fmt l = Format.fprintf fmt "@[[%a]@]" (Blist.pp pp_semicolonsp T.pp) l
  let to_string = mk_to_string pp
end
