open Misc

module Make (T : Utilsigs.BasicType) :
  Utilsigs.OrderedContainer with type elt = T.t = struct
  module S = Set.Make (T)
  include S
  include Fixpoint.Make (S)

  let equal s s' = s == s' || equal s s'
  let compare s s' = if s == s' then 0 else compare s s'

  let hash_fold_t state s =
    fold
      (fun el st -> T.hash_fold_t st el)
      s
      (Ppx_hash_lib.Std.Hash.fold_int state (cardinal s))

  let hash t = Ppx_hash_lib.Std.Hash.run hash_fold_t t
  let map_to oadd oempty f s = fold (fun el s' -> oadd (f el) s') s oempty
  let opt_map_to oadd oempty f s = map_to (Option.dest Fun.id oadd) oempty f s
  let map_to_list f s = Blist.rev (map_to Blist.cons [] f s)
  let weave split tie join xs acc = Blist.weave split tie join (elements xs) acc
  let union_of_list l = Blist.fold_left (fun s i -> union s i) empty l
  let find_suchthat_opt f s = Seq.find f (to_seq s)

  let find_suchthat f s =
    match find_suchthat_opt f s with Some x -> x | None -> raise Not_found

  let find_map (type a) (f : elt -> a option) (s : t) =
    let exception Found of a option in
    try
      iter
        (fun x ->
          match f x with None -> () | some_result -> raise (Found some_result))
        s;
      None
    with Found some_result -> some_result

  let count p s = fold (fun x n -> if p x then n + 1 else n) s 0

  let pp fmt s =
    Format.fprintf fmt "@[{%a}@]" (Blist.pp pp_commasp T.pp) (to_list s)

  let to_string s = "{" ^ Blist.to_string ", " T.to_string (to_list s) ^ "}"

  let rec subsets s =
    if is_empty s then [ empty ]
    else
      let x = choose s in
      let s = remove x s in
      let xxs = subsets s in
      xxs @ Blist.map (add x) xxs

  let del_first p s =
    match find_suchthat_opt p s with None -> s | Some x -> remove x s

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
