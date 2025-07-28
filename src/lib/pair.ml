open Misc

let mk x y = (x, y)

let left (x, _) = x

let right (_, y) = y

let map f p = (f (fst p), f (snd p))

let map_left f p = (f (fst p), snd p)

let map_right f p = (fst p, f (snd p))

let apply f p = f (fst p) (snd p)

let conj p = apply ( && ) p

let disj p = apply ( || ) p

let swap (x, y) = (y, x)

let perm f p = apply f p || apply f (swap p)

let fold f (x, y) a = f y (f x a)

let both = conj

let either = disj

let pp pp_fst pp_snd fmt (fst, snd) =
  Format.fprintf fmt "@[(%a,@ %a)@]" pp_fst fst pp_snd snd

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

  let pp = pp T.pp S.pp

  let to_string = mk_to_string pp
end
