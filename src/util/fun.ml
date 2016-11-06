(** Combinators for manipulating functions. *)

type 'a predicate = 'a -> bool

let _true _ = true
let _false _ = false

let neg f x = not (f x)
let conj f g x = (f x) && (g x)
let list_conj fs x = (List.fold_right conj fs _true) x
(* let list_conj fs x = List.fold_left (fun v f -> v && (f x)) true fs *) 
let disj f g x = (f x) || (g x)
let list_disj fs x = (List.fold_right disj fs _false) x
(* let list_disj fs x = List.fold_left (fun v f -> v || (f x)) true fs *) 
let id x = x
let uncurry f (x, y) = f x y
let curry f x y = f (x, y)
let swap f x y = f y x

let direct inverse = 
  if not inverse then 
    id
  else
    swap
  
let rec iter f n x = match n with
  | n when n <= 0 -> x
  | n -> iter f (n-1) (f x) 