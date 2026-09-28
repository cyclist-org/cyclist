open Misc

module Make (T : Utilsigs.BasicType) :
  Utilsigs.BasicType with type t = T.t list = struct
  type t = T.t list [@@deriving compare, equal]

  let hash l = Blist.fold_left (fun h v -> genhash h (T.hash v)) 0x9e3779b9 l
  let pp fmt l = Format.fprintf fmt "@[[%a]@]" (Blist.pp pp_semicolonsp T.pp) l
  let to_string = mk_to_string pp
end
