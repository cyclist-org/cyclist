include List

let foldl = fold_left
let foldr = fold_right
let empty = []
let of_list l = l
let to_list l = l

let rec del_first p = function
  | [] -> []
  | x :: xs -> if p x then xs else x :: del_first p xs

let to_string sep conv xs = String.concat sep (map conv xs)

let rec pp pp_sep pp_elem fmt = function
  | [] -> ()
  | [ h ] -> Format.fprintf fmt "%a" pp_elem h
  | h :: t ->
      Format.fprintf fmt "%a%a%a" pp_elem h pp_sep () (pp pp_sep pp_elem) t

let decons = function x :: xs -> (x, xs) | _ -> invalid_arg "decons"

let repeat a n =
  if Stdlib.( < ) n 0 then invalid_arg "repeat"
  else
    let rec aux a acc = function 0 -> acc | m -> aux a (a :: acc) (m - 1) in
    aux a [] n

let rev_filter p xs = foldl (fun acc x -> if p x then x :: acc else acc) [] xs
let rec but_last = function [ _ ] | [] -> [] | x :: xs -> x :: but_last xs
let range n xs = mapi (fun m _ -> m + n) xs

let remove_nth n l =
  let rec remove_nth n l acc =
    match l with
    | [] -> invalid_arg "Blist.remove_nth"
    | y :: ys -> begin
        match n with
        | 0 -> rev_append acc ys
        | _ -> remove_nth (n - 1) ys (y :: acc)
      end
  in
  remove_nth n l []

let replace_nth z n l =
  let rec replace_nth n l acc =
    match l with
    | [] -> invalid_arg "Blist.replace_nth"
    | x :: xs -> begin
        match n with
        | 0 -> rev_append acc (z :: xs)
        | _ -> replace_nth (n - 1) xs (x :: acc)
      end
  in
  replace_nth n l []

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
  | [ x ] -> tie x acc
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
    [ [] ] lol

let rec pairs = function
  | [] | [ _ ] -> []
  | x :: (x' :: _ as xs) -> (x, x') :: pairs xs

let map_to oadd oempty f xs = foldl (fun ys z -> oadd (f z) ys) oempty xs

let opt_map_to oadd oempty f xs =
  map_to (function None -> Fun.id | Some x -> oadd x) oempty f xs
