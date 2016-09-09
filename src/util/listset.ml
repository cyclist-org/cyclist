module Make(T: Utilsigs.BasicType) : Utilsigs.OrderedContainer with type elt = T.t =
  struct
    module MSet = Listmultiset.Make(T)
    include MSet
    include Fixpoint.Make(MSet)
    
    let rec uniq = function
      | [] | [_] as l -> l
      | x::((x'::_) as tl) -> match T.compare x x' with
        | 0 -> uniq tl
        | i when i<0 -> x::(uniq tl)
        | _ -> failwith "uniq"

    let of_list l = uniq (Blist.fast_sort T.compare l)
    let union xs ys = uniq (Blist.merge T.compare xs ys)
    let union_of_list l = uniq (Blist.fold_left union [] l)
    let endomap f xs = of_list (Blist.map f xs)

    let rec add x = function
      | [] -> [x]
      | (y::ys) as zs -> match T.compare x y with
        | 0 -> zs
        | n when n < 0 -> x::zs
        | _ -> y::(add x ys)

    let rec subsets xs =
      if is_empty xs then [empty] else
      let x = choose xs in
      let xs = remove x xs in
      let xxs = subsets xs in
      xxs @ (Blist.map (fun y -> add x y) xxs)

  end
