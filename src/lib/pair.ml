open Misc

let mk x y = (x, y)
let left (x, _) = x
let right (_, y) = y
let map f p = (f (fst p), f (snd p))
let apply f p = f (fst p) (snd p)
let conj p = apply ( && ) p
let disj p = apply ( || ) p
let swap (x, y) = (y, x)
let perm f p = apply f p || apply f (swap p)
let fold f (x, y) a = f y (f x a)
let both = conj
let either = disj

module Make (T : Utilsigs.BasicType) (S : Utilsigs.BasicType) :
  Utilsigs.BasicType with type t = T.t * S.t = struct
  type t = T.t * S.t [@@deriving compare, equal]

  let hash (i : t) = genhash (T.hash (fst i)) (S.hash (snd i))
  let pp fmt (i, j) = Format.fprintf fmt "@[(%a,@ %a)@]" T.pp i S.pp j
  let to_string = mk_to_string pp
end
