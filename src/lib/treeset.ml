open Misc

module Make (T : Utilsigs.BasicType) :
  Utilsigs.OrderedContainer with type elt = T.t = struct
  include Set.Make (T)
  include Fixpoint.Make (Set.Make (T))

  let of_list l = Blist.fold_left (fun a b -> add b a) empty l

  let map_to oadd oempty f s = fold (fun el s' -> oadd (f el) s') s oempty

  let opt_map_to oadd oempty f s = map_to (Option.dest Fun.id oadd) oempty f s

  let map_to_list f s = Blist.rev (map_to Blist.cons [] f s)

  let weave split tie join xs acc =
    Blist.weave split tie join (elements xs) acc

  let union_of_list l = Blist.fold_left (fun s i -> union s i) empty l

  let find_suchthat_opt f s =
    Seq.find f (to_seq s)

  let find_suchthat f s =
    match (find_suchthat_opt f s) with
    | Some x ->
      x
    | None ->
      raise Not_found

  let find_map f s =
    Option.flatten
      (Option.map f (find_suchthat_opt (fun x -> Option.is_some (f x)) s))

  let count p s = fold (fun x n -> if (p x) then n + 1 else n) s 0

  let to_list = elements

  let pp fmt s =
    Format.fprintf fmt "@[{%a}@]" (Blist.pp pp_commasp T.pp) (to_list s)

  let to_string s = "{" ^ Blist.to_string ", " T.to_string (to_list s) ^ "}"

  let hash s = fold (fun x h -> genhash (T.hash x) h) s 0x9e3779b9

  let rec subsets s =
    if is_empty s then [empty]
    else
      let x = choose s in
      let s = remove x s in
      let xxs = subsets s in
      xxs @ Blist.map (add x) xxs

  let del_first p s =
    match find_suchthat_opt p s with
    | None ->
      s
    | Some x ->
      remove x s

  let disjoint xs ys =
    let xs = to_list xs in
    let ys = to_list ys in
    let rec disjoint xs ys =
      match (xs, ys) with
      | [], _ -> true
      | _, [] -> true
      | x :: xs, y :: ys -> (
        match T.compare x y with
        | 0 -> false
        | n when Stdlib.( < ) n 0 -> disjoint xs (y :: ys)
        | _ -> disjoint (x :: xs) ys )
    in
    disjoint xs ys

  include Unification.MakeUnifier (struct
    type t = Set.Make(T).t

    type elt = T.t

    let empty = empty

    let is_empty = is_empty

    let equal = equal

    let add = add

    let choose = choose

    let remove = remove

    let find_map = find_map
  end)
end
