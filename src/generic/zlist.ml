open Lib

type 'a znode =
  | Empty
  | Node of 'a * 'a t
and 'a t = ('a znode) Lazy.t

let empty = Lazy.lazy_from_val (Empty)

let is_empty l = match Lazy.force l with
  | Empty -> true
  | Node _ -> false

let hd l = match Lazy.force l with
  | Empty -> invalid_arg "Zlist.hd"
  | Node(x,_) -> x

let tl l = match Lazy.force l with
  | Empty -> invalid_arg "Zlist.tl"
  | Node(_,l') -> l'

let cons z zs = Lazy.lazy_from_val (Node(z, zs))

let rec of_list l = lazy (
  match l with
    | [] -> Empty
    | x::xs -> Node(x, of_list xs)
)

let filter p l =
  let rec next_true l = match Lazy.force l with
    (* Compute the next accepted predicate without thunkification *)
    | Node(x, l) when not (p x) -> next_true l
    | l -> l
  in
  let rec aux l = lazy (
    match next_true l with
    | Node(x, l) -> Node(x, aux l)
    | Empty      -> Empty
  ) in
  aux l

let rec map (f:'a->'b) (l:'a t) =  lazy (
  match Lazy.force l with
  | Empty -> Empty
  | Node(x,l') -> Node(f x, map f l')
)

let rec to_list l = match Lazy.force l with
  | Empty -> []
  | Node(x,xs) -> x::(to_list xs)

let rec find_some f l = match Lazy.force l with
  | Empty -> None
  | Node(x,xs) -> match f x with
    | None -> find_some f xs
    | y -> y

let rec from_while f = lazy (
  match f () with
    | None  -> Empty
    | Some x -> Node (x, from_while f)
)

(* let append (l1 : 'a t) (l2 : 'a t) =                   *)
(*   let rec aux list =  match Lazy.force list with       *)
(*       | Node (x, (t : 'a t)) -> Node (x, lazy (aux t)) *)
(*       | _                    -> Lazy.force l2          *)
(*   in lazy (aux l1)                                     *)

let rec flatten (l : 'a t t) : 'a t = lazy (
  match Lazy.force l with
    | Empty -> Empty
    | Node(x, xs) -> match Lazy.force x with
      | Empty -> Lazy.force (flatten xs)
      | Node(y, ys) ->
        Node(y, flatten (Lazy.lazy_from_val (Node(ys,xs))))
)

(* let choose = function                                                  *)
(*   | [] -> [[]]                                                         *)
(*   | hd::tl -> let c = choose tl in                                     *)
(*     Blist.flatten (Blist.map (fun el -> Blist.map (fun l -> el::l) c) hd) *)

let rec choose (l : 'a t t) : 'a t t = lazy (
  match Lazy.force l with
    | Empty -> Node(empty, empty)
    | Node(x, xs) ->
      let cs = choose xs in
      Lazy.force (flatten (map (fun y -> map (cons y) cs) x))
  )

let find_first p (l : 'a t) = find_some (Option.pred p) l

let rec fold f ys a = match Lazy.force ys with
  | Empty -> a
  | Node(x, xs) -> fold f xs (f a x)

let get_opt l = map Option.get (filter Option.is_some l)

(* module Zlist =                          *)
(*   struct                                *)
(*     include Blist                        *)
(*     type 'a t = 'a list                 *)
(*     let empty = []                      *)
(*     let of_list l = l                   *)
(*     let to_list l = l                   *)
(*     let is_empty l = (l=[])             *)
(*     let rec Blist.find_some f = function      *)
(*       | [] -> None                      *)
(*       | hd::tl -> match f hd with       *)
(*         | None -> Blist.find_some f tl        *)
(*         | x -> x                        *)
(*     let Blist.find_first p l = Blist.find_first p l *)
(*     let from_hd = function              *)
(*       | [] -> []                        *)
(*       | hd::_ -> [hd]                   *)
(*     let from_fun f = f ()               *)
(*   end                                   *)
