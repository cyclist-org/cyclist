type 'a znode = Empty | Node of 'a * 'a t

and 'a t = 'a znode Lazy.t

let empty = Lazy.from_val Empty

let is_empty l = match Lazy.force l with Empty -> true | Node _ -> false

let hd l =
  match Lazy.force l with Empty -> invalid_arg "Zlist.hd" | Node (x, _) -> x

let tl l =
  match Lazy.force l with
  | Empty -> invalid_arg "Zlist.tl"
  | Node (_, l') -> l'

let cons z zs = Lazy.from_val (Node (z, zs))

let singleton x = Lazy.from_val (Node (x, empty))

let rec of_list l =
  lazy (match l with [] -> Empty | x :: xs -> Node (x, of_list xs))

let filter p l =
  let rec next_true l =
    match Lazy.force l with
    (* Compute the next accepted predicate without thunkification *)
    | Node (x, l) when not (p x) -> next_true l
    | l -> l
  in
  let rec aux l =
    lazy
      (match next_true l with Node (x, l) -> Node (x, aux l) | Empty -> Empty)
  in
  aux l

let rec map (f : 'a -> 'b) (l : 'a t) =
  lazy
    ( match Lazy.force l with
    | Empty -> Empty
    | Node (x, l') -> Node (f x, map f l') )

let rec to_list l =
  match Lazy.force l with Empty -> [] | Node (x, xs) -> x :: to_list xs

let rec find_map f l =
  match Lazy.force l with
  | Empty -> None
  | Node (x, xs) -> ( match f x with None -> find_map f xs | y -> y )

let rec from_while f =
  lazy (match f () with None -> Empty | Some x -> Node (x, from_while f))

(* let append (l1 : 'a t) (l2 : 'a t) =                   *)
(*   let rec aux list =  match Lazy.force list with       *)
(*       | Node (x, (t : 'a t)) -> Node (x, lazy (aux t)) *)
(*       | _                    -> Lazy.force l2          *)
(*   in lazy (aux l1)                                     *)

let rec flatten (l : 'a t t) : 'a t =
  lazy
    ( match Lazy.force l with
    | Empty -> Empty
    | Node (x, xs) -> (
      match Lazy.force x with
      | Empty -> Lazy.force (flatten xs)
      | Node (y, ys) -> Node (y, flatten (Lazy.from_val (Node (ys, xs))))
      ) )

let bind f zs = flatten (map f zs)

let rec choose (l : 'a t t) : 'a t t =
  lazy
    ( match Lazy.force l with
    | Empty -> Node (empty, empty)
    | Node (x, xs) ->
        let cs = choose xs in
        Lazy.force (bind (fun y -> map (cons y) cs) x) )

let find_opt p (l : 'a t) = find_map (Option.pred p) l

let rec fold f ys a =
  match Lazy.force ys with Empty -> a | Node (x, xs) -> fold f xs (f a x)

let fold_left f a ys = fold f ys a

let get_opt l = map Option.get (filter Option.is_some l)
