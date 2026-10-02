module Make (T : Utilsigs.BasicType) :
  Utilsigs.OrderedContainer with type elt = T.t = struct
  module MSet = Listmultiset.Make (T)
  include MSet
  include Fixpoint.Make (MSet)

  let of_list l = Blist.sort_uniq T.compare l

  (* linear scan instead of merge and then sort *)
  let union xs ys =
    let rec merge_dedup xs ys =
      match (xs, ys) with
      | [], zs | zs, [] -> zs
      | x :: xs', y :: ys' -> (
          match T.compare x y with
          | 0 -> x :: merge_dedup xs' ys'
          | n when n < 0 -> x :: merge_dedup xs' ys
          | _ -> y :: merge_dedup xs ys')
    in
    merge_dedup xs ys

  (* same logic as in Listmultiset.union_of_list *)
  let union_of_list l =
    let rec merge_pairs = function
      | ([] | [ _ ]) as l -> l
      | x :: y :: rest -> union x y :: merge_pairs rest
    in
    let rec loop = function
      | [] -> []
      | [ x ] -> x
      | l -> loop (merge_pairs l)
    in
    loop l

  let map f xs = of_list (Blist.map f xs)

  let rec add x = function
    | [] -> [ x ]
    | y :: ys as zs -> (
        match T.compare x y with
        | 0 -> zs
        | n when Int.( < ) n 0 -> x :: zs
        | _ -> y :: add x ys)

  let rec subsets xs =
    if is_empty xs then [ empty ]
    else
      let x = choose xs in
      let xs = remove x xs in
      let xxs = subsets xs in
      xxs @ Blist.map (fun y -> add x y) xxs
end
