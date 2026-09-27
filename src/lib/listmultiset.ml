module Make (T : Utilsigs.BasicType) = struct
  type elt = T.t

  include Blist
  include Flist.Make (T)

  let of_list l = Blist.fast_sort T.compare l
  let min_elt = Blist.hd
  let elements = to_list

  let rec add x = function
    | [] -> [ x ]
    | y :: ys as zs -> (
        match T.compare x y with
        | n when Stdlib.( <= ) n 0 -> x :: zs
        | _ -> y :: add x ys)

  let fold f xs a = Blist.fold_left (fun y a' -> f a' y) a xs
  let cardinal = Blist.length
  let choose = min_elt
  let union xs ys = Blist.merge T.compare xs ys
  let union_of_list l = Blist.fold_left union [] l
  let map f xs = of_list (Blist.map f xs)

  let rec mem x = function
    | [] -> false
    | y :: ys -> (
        match T.compare x y with 0 -> true | n -> Stdlib.( > ) n 0 && mem x ys)

  let rec max_elt = function
    | [] -> raise Not_found
    | [ x ] -> x
    | _ :: xs -> max_elt xs

  (* only removes first occurence *)
  let rec remove x = function
    | [] -> []
    | y :: ys as zs -> (
        match T.compare x y with
        | 0 -> ys
        | n when Stdlib.( > ) n 0 -> y :: remove x ys
        | _ -> zs)

  let rec inter xs ys =
    match (xs, ys) with
    | [], _ | _, [] -> []
    | w :: ws, z :: zs -> (
        match T.compare w z with
        | 0 -> w :: inter ws zs
        | n when Stdlib.( > ) n 0 -> inter xs zs
        | _ -> inter ws ys)

  let rec subset xs ys =
    match (xs, ys) with
    | [], _ -> true
    | _, [] -> false
    | w :: ws, z :: zs -> (
        match T.compare w z with
        | 0 -> subset ws zs
        | n when Stdlib.( > ) n 0 -> subset xs zs
        | _ -> false)

  let rec diff xs ys =
    match (xs, ys) with
    | [], _ -> []
    | _, [] -> xs
    | w :: ws, z :: zs -> (
        match T.compare w z with
        | 0 -> diff ws zs
        | n when Stdlib.( < ) n 0 -> w :: diff ws ys
        | _ -> diff xs zs)

  let split x xs =
    let rec div ys = function
      | [] -> (ys, false, [])
      | z :: zs as ws -> (
          match T.compare x z with
          | 0 -> (ys, true, ws)
          | n when Stdlib.( > ) n 0 -> div (z :: ys) zs
          | _ -> (ys, false, ws))
    in
    let l, f, r = div [] xs in
    (Blist.rev l, f, r)

  let map_to_list f xs = Blist.map f xs
  let to_rev_seq xs = to_seq (rev xs)
  let find_suchthat = find
  let find_first = find
  let find_suchthat_opt = find_opt
  let find_first_opt = find_opt

  let rec find x = function
    | [] -> raise Not_found
    | x' :: xs -> (
        match T.compare x x' with
        | 0 -> x'
        | n when n > 0 -> find x xs
        | _ -> raise Not_found)

  let find_opt x xs = try Some (find x xs) with Not_found -> None
  let count p s = fold (fun x n -> if p x then n + 1 else n) s 0

  let rec subsets xs =
    if is_empty xs then [ empty ]
    else
      let x = choose xs in
      let xs = remove x xs in
      let xxs = subsets xs in
      xxs @ Blist.map (fun y -> add x y) xxs

  let rec disjoint xs ys =
    match (xs, ys) with
    | [], _ -> true
    | _, [] -> true
    | x :: xs, y :: ys -> (
        match T.compare x y with
        | 0 -> false
        | n when Stdlib.( < ) n 0 -> disjoint xs (y :: ys)
        | _ -> disjoint (x :: xs) ys)

  let find_last_opt _ _ = failwith "Not implemented!"
  let find_last _ _ = failwith "Not implemented!"
  let choose_opt _ = failwith "Not implemented!"
  let max_elt_opt _ = failwith "Not implemented!"
  let min_elt_opt _ = failwith "Not implemented!"
  let add_seq _ _ = failwith "Not implemented!"
  let to_seq_from _ _ = failwith "Not implemented!"

  include Fixpoint.Make (struct
    type nonrec t = t

    let equal = equal
  end)

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
