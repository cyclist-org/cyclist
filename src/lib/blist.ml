include List

let foldl = fold_left

let foldr = fold_right

let empty = []

let is_empty = function [] -> true | _ -> false

let of_list l = l

let to_list l = l

let singleton x = [x]

let rec del_first p = function
  | [] -> []
  | x :: xs -> if p x then xs else x :: del_first p xs

let to_string sep conv xs = String.concat sep (map conv xs)

let rec pp pp_sep pp_elem fmt = function
  | [] -> ()
  | [h] -> Format.fprintf fmt "%a" pp_elem h
  | h :: t ->
      Format.fprintf fmt "%a%a%a" pp_elem h pp_sep () (pp pp_sep pp_elem) t

let decons = function x :: xs -> (x, xs) | _ -> invalid_arg "decons"

let repeat a n =
  if Stdlib.( < ) n 0 then invalid_arg "repeat"
  else
    let rec aux a acc = function 0 -> acc | m -> aux a (a :: acc) (m - 1) in
    aux a [] n

let rev_filter p xs = foldl (fun acc x -> if p x then x :: acc else acc) [] xs

let rec but_last = function [_] | [] -> [] | x :: xs -> x :: but_last xs

let range n xs = mapi (fun m _ -> m + n) xs

let rec remove_nth n = function
  | [] -> invalid_arg "Blist.remove_nth"
  | y :: ys -> ( match n with 0 -> ys | _ -> y :: remove_nth (n - 1) ys )

let replace_nth z n xs =
  if Stdlib.( < ) n 0 then invalid_arg "Blist.replace_nth"
  else
    let f m y = if Stdlib.( = ) n m then z else y in
    mapi f xs

let rec take n l =
  match (l, n) with
  | _, 0 -> []
  | [], _ -> invalid_arg "Blist.take"
  | x :: xs, _ -> x :: take (n - 1) xs

let rec drop n l =
  match (l, n) with
  | _, 0 -> l
  | [], _ -> invalid_arg "Blist.drop"
  | _ :: xs, _ -> drop (n - 1) xs

let indexes xs = range 0 xs

(* This exists in OCaml's List module only from version 4.10 *)
let rec find_map f = function
  | [] -> None
  | x :: xs -> ( match f x with None -> find_map f xs | y -> y )

let find_index p l =
  let rec aux p n = function
    | [] -> raise Not_found
    | x :: xs -> if p x then n else aux p (n + 1) xs
  in
  aux p 0 l

let find_indexes p xs =
  let rec aux n = function
    | [] -> []
    | y :: ys -> if p y then n :: aux (n + 1) ys else aux (n + 1) ys
  in
  aux 0 xs

let rec equal eq xs ys =
  match (xs, ys) with
  | [], [] -> true
  | x :: xs, y :: ys -> eq x y && equal eq xs ys
  | _ -> false

let rec prepend_to_all x = function
  | [] -> []
  | y :: ys -> x :: y :: prepend_to_all x ys

let intersperse x = function [] -> [] | y :: ys -> y :: prepend_to_all x ys

let rec unzip3 = function
  | [] -> ([], [], [])
  | (x, y, z) :: ws ->
      let xs, ys, zs = unzip3 ws in
      (x :: xs, y :: ys, z :: zs)

let rec zip3 xs ys zs =
  match (xs, ys, zs) with
  | [], [], [] -> []
  | b :: bs, c :: cs, d :: ds -> (b, c, d) :: zip3 bs cs ds
  | _ -> invalid_arg "zip3"

let cartesian_product xs ys =
  foldl (fun acc x -> foldl (fun acc' y -> (x, y) :: acc') acc ys) [] xs

let cartesian_hemi_square xs =
  let rec chs acc = function
    | [] -> acc
    | el :: tl -> chs (fold_left (fun acc' el' -> (el, el') :: acc') acc tl) tl
  in
  chs [] xs

let bind f xs = flatten (map f xs)

let rec uniq eq = function
  | [] -> []
  | x :: xs -> x :: uniq eq (filter (fun x' -> not (eq x x')) xs)

let rec weave split tie join xs acc =
  match xs with
  | [] -> join []
  | [x] -> tie x acc
  | hd :: tl -> join (map (weave split tie join tl) (split hd acc))

(* let rec choose = function                    *)
(*   | [] -> [[]]                               *)
(*   | xs::ys ->                                *)
(*     let choices = choose ys in               *)
(*     bind (fun x -> map (cons x) choices) xs  *)

(* tail rec for satisfiability algo *)
let choose lol =
  let _, lol =
    foldl
      (fun (r, a) l -> (not r, (if r then rev l else l) :: a))
      (true, []) lol
  in
  foldl
    (fun ll -> foldl (fun tl e -> foldl (fun t l -> (e :: l) :: t) tl ll) [])
    [[]] lol

let rec _combs k len l =
  if Stdlib.( = ) k 0 then [[]]
  else if Stdlib.( < ) len k then []
  else if Stdlib.( = ) k len then [l]
  else
    let h, t = (hd l, tl l) in
    let starting_with_h =
      map (fun sublist -> h :: sublist) (_combs (pred k) (pred len) t)
    in
    starting_with_h @ _combs k (pred len) t

let combs k l = _combs k (length l) l

let rec pairs = function
  | [] | [_] -> []
  | x :: (x' :: _ as xs) -> (x, x') :: pairs xs

let map_to oadd oempty f xs = foldl (fun ys z -> oadd (f z) ys) oempty xs

let opt_map_to oadd oempty f xs =
  map_to (function None -> Fun.id | Some x -> oadd x) oempty f xs

(* tail rec versions, generally slower *)

(* let map f xs = rev (rev_map f xs)                                    *)
(* let map2 f xs ys = rev (rev_map2 f xs ys)                            *)
(* let append xs ys = rev_append (rev xs) ys                            *)
(* let flatten xs = rev (fold_left (fun ys x -> rev_append x ys) [] xs) *)
(* let but_last = function     *)
(*   | [] -> []                *)
(*   | xs -> rev (tl (rev xs)) *)
(* let range n xs =                                             *)
(*   rev (snd (foldl (fun (m,ys) _ -> (m+1, m::ys)) (n,[]) xs)) *)
(* let remove_nth n xs =                                                            *)
(*   if n<0 then invalid_arg "Blist.remove_nth" else                                *)
(*   rev (snd (foldl (fun (m,ys) x -> (m+1, if m=n then ys else x::ys)) (0,[]) xs)) *)
(* let replace_nth z n xs =                                                            *)
(*   if n<0 then invalid_arg "Blist.replace_nth" else                                  *)
(*   rev (snd (foldl (fun (m,ys) x -> (m+1, if m=n then z::ys else x::ys)) (0,[]) xs)) *)
(* let find_indexes p xs =                                                          *)
(*   rev (snd (foldl (fun (m,ms) x -> (m+1, if p x then m::ms else ms)) (0,[]) xs)) *)
(* let unzip3 xs =                                                                *)
(*   let (bs,cs,ds) =                                                             *)
(*     foldl (fun (bs,cs,ds) (b,c,d) -> (b::bs, c::cs, d::ds)) ([], [], []) xs in *)
(*   (rev bs, rev cs, rev ds)                                                     *)
(* let zip3 xs' ys' zs' =                                         *)
(*   let rec aux acc xs ys zs =                                   *)
(*     match (xs, ys, zs) with                                    *)
(*       | ([], [], []) -> acc                                    *)
(*       | (b::bs, c::cs, d::ds) -> aux ((b,c,d)::acc) bs cs ds   *)
(*       | _ -> invalid_arg "zip3" in                             *)
(*   rev (aux [] xs' ys' zs')                                     *)
(* let bind f xs = rev (fold_left (fun ys x -> rev_append (f x) ys) [] xs) *)
