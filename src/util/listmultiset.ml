open Lib

module Make (T : Utilsigs.BasicType) = struct
  type elt = T.t

  include Flist.Make (T)

  let empty = []

  let is_empty = function [] -> true | _ -> false

  let singleton x = [x]

  let of_list l = Blist.fast_sort T.compare l

  let to_list xs = xs

  let min_elt = Blist.hd

  let elements = to_list

  let rec add x = function
    | [] -> [x]
    | y :: ys as zs -> (
      match T.compare x y with
      | n when Pervasives.( <= ) n 0 -> x :: zs
      | _ -> y :: add x ys )

  let fold f xs a = Blist.fold_left (fun y a' -> f a' y) a xs

  let cardinal = Blist.length

  let choose = min_elt

  let union xs ys = Blist.merge T.compare xs ys

  let union_of_list l = Blist.fold_left union [] l

  let endomap f xs = of_list (Blist.map f xs)

  let partition = Blist.partition

  let find = Blist.find

  let filter = Blist.filter

  let exists = Blist.exists

  let for_all = Blist.for_all

  let iter = Blist.iter

  let rec mem x = function
    | [] -> false
    | y :: ys -> (
      match T.compare x y with
      | 0 -> true
      | n -> Pervasives.( > ) n 0 && mem x ys )

  let rec max_elt = function
    | [] -> raise Not_found
    | [x] -> x
    | _ :: xs -> max_elt xs

  let rec del_first p = function
    | [] -> []
    | y :: ys -> if p y then ys else y :: del_first p ys

  (* only removes first occurence *)
  let rec remove x = function
    | [] -> []
    | y :: ys as zs -> (
      match T.compare x y with
      | 0 -> ys
      | n when Pervasives.( > ) n 0 -> y :: remove x ys
      | _ -> zs )

  let rec inter xs ys =
    match (xs, ys) with
    | [], _ | _, [] -> []
    | w :: ws, z :: zs -> (
      match T.compare w z with
      | 0 -> w :: inter ws zs
      | n when Pervasives.( > ) n 0 -> inter xs zs
      | _ -> inter ws ys )

  let rec subset xs ys =
    match (xs, ys) with
    | [], _ -> true
    | _, [] -> false
    | w :: ws, z :: zs -> (
      match T.compare w z with
      | 0 -> subset ws zs
      | n when Pervasives.( > ) n 0 -> subset xs zs
      | _ -> false )

  let rec diff xs ys =
    match (xs, ys) with
    | [], _ -> []
    | _, [] -> xs
    | w :: ws, z :: zs -> (
      match T.compare w z with
      | 0 -> diff ws zs
      | n when Pervasives.( < ) n 0 -> w :: diff ws ys
      | _ -> diff xs zs )

  let split x xs =
    let rec div ys = function
      | [] -> (ys, false, [])
      | z :: zs as ws -> (
        match T.compare x z with
        | 0 -> (ys, true, ws)
        | n when Pervasives.( > ) n 0 -> div (z :: ys) zs
        | _ -> (ys, false, ws) )
    in
    let l, f, r = div [] xs in
    (Blist.rev l, f, r)

  let map_to oadd oempty f xs = fold (fun z ys -> oadd (f z) ys) xs oempty

  let opt_map_to oadd oempty f xs =
    map_to (Option.dest Fun.id oadd) oempty f xs

  let map_to_list f xs = Blist.map f xs

  let weave = Blist.weave

  let rec find_map f = function
    | [] -> None
    | x :: xs -> ( match f x with None -> find_map f xs | r -> r )

  let find_opt f xs = find_map (fun x -> if f x then Some x else None) xs

  let rec subsets xs =
    if is_empty xs then [empty]
    else
      let x = choose xs in
      let xs = remove x xs in
      let xxs = subsets xs in
      xxs @ Blist.map (fun y -> add x y) xxs

  let rec disjoint xs ys =
    match (xs, ys) with
    | [], ys -> true
    | xs, [] -> true
    | x :: xs, y :: ys -> (
      match T.compare x y with
      | 0 -> false
      | n when Pervasives.( < ) n 0 -> disjoint xs (y :: ys)
      | _ -> disjoint (x :: xs) ys )

  let find_last_opt _ _ = assert false

  let find_last _ _ = assert false

  let find_first_opt _ _ = assert false

  let find_first _ _ = assert false

  let choose_opt _ = assert false

  let max_elt_opt _ = assert false

  let min_elt_opt _ = assert false

  let map _ _ = assert false

  let of_seq _ = assert false

  let add_seq _ _ = assert false

  let to_seq _ = assert false

  let to_seq_from _ _ = assert false

  include Unification.MakeUnifier (struct
    type t = Flist.Make(T).t

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
