module Make (T : Utilsigs.BasicType) :
  Utilsigs.OrderedContainer with type elt = T.t = struct
  module MSet = Listmultiset.Make (T)
  include MSet
  include Fixpoint.Make (MSet)

  let of_list l = Blist.sort_uniq T.compare l
  let union xs ys = of_list (Blist.merge T.compare xs ys)

  let union_of_list l =
    of_list (Blist.fold_left (fun acc x -> Blist.merge T.compare x acc) [] l)

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
