open Lib

let mk x y = (x, y)

let left (x, y) = x

let right (x, y) = y

let map f p = (f (fst p), f (snd p))

let map_left f p = (f (fst p), snd p)

let map_right f p = (fst p, f (snd p))

let apply f p = f (fst p) (snd p)

let conj p = apply ( && ) p

let disj p = apply ( || ) p

let swap (x, y) = (y, x)

let fold f (x, y) a = f y (f x a)

let both = conj

let either = disj

module Make (T : Utilsigs.BasicType) (S : Utilsigs.BasicType) :
  Utilsigs.BasicType with type t = T.t * S.t = struct
  type t = T.t * S.t

  let compare i j =
    if i == j then 0
    else
      match T.compare (fst i) (fst j) with
      | 0 -> S.compare (snd i) (snd j)
      | n -> n

  let equal i j = i == j || (T.equal (fst i) (fst j) && S.equal (snd i) (snd j))

  let hash (i : t) = genhash (T.hash (fst i)) (S.hash (snd i))

  let pp fmt (i, j) = Format.fprintf fmt "@[(%a,@ %a)@]" T.pp i S.pp j

  let to_string = mk_to_string pp
end
