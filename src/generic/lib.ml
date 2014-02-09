(* let list_tl_map f l = Array.to_list (Array.map f (Array.of_list l)) *)

let do_debug = ref false
let debug f = if !do_debug then print_endline (f ()) else ()

let string_of_list sep conv ls = String.concat sep (Blist.map conv ls)

let cons hd tl = hd::tl
let decons = function
  | [] -> failwith "decons"
  | x::xs -> (x,xs)

let pp_comma fmt () =
  Format.pp_print_char fmt ','

let pp_semicolonsp fmt () =
  Format.pp_print_char fmt ';' ; Format.pp_print_space fmt ()

let pp_commasp fmt () =
  pp_comma fmt () ; Format.pp_print_space fmt ()

let rec pp_list pp_sep pp_elem fmt = function
  | [] -> ()
  | [h] -> Format.fprintf fmt "%a" pp_elem h
  | h::t ->
    Format.fprintf fmt "%a%a%a" pp_elem h pp_sep () (pp_list pp_sep pp_elem) t

let repeat a n =
  let rec aux a acc = function
    | 0 -> acc
    | m -> aux a (a::acc) (m-1) in
  aux a [] n

let rev_filter p xs =
	let rec aux p acc = function
  	| [] -> acc
  	| y::ys -> if p y then aux p (y::acc) ys else aux p acc ys in
	aux p [] xs

let rec but_last = function
  | [] | [_] -> []
  | x::xs -> x::(but_last xs)

let rec remove_nth n = function
	| [] -> invalid_arg "remove_nth"
	| x::xs -> match n with
    | 0 -> xs
    | n -> x::(remove_nth (n-1) xs)

let rec range n = function
  | [] -> []
  | _::xs -> n::(range (n+1) xs)


let rec forloop n m f a =
	if n=m then a else forloop (n+1) m f (f a)

(* name near-clash with indexes below *)
let indices l = range 0 l

let cartesian_product xs ys =
  Blist.fold_left (fun acc x ->
    Blist.fold_left (fun acc y ->
      (x,y) :: acc)
      acc ys)
    [] xs

let cartesian_hemi_square xs =
  let rec chs acc = function
    | [] -> acc
    | el::tl ->
      chs (Blist.fold_left (fun acc' el' -> (el,el')::acc') acc tl) tl
  in chs [] xs


(* accept a list of lists and return another list of lists such that *)
(* each list in the output contains exactly one member from each list in the input *)
(* NB maintain the order of elements from the input list *)
(* this allows a heuristic in proof search where the subgoals most likely to fail *)
(* appear earlier in the list of subgoals *)
(* let cc l =                                     *)
(*   let choose_one xs ys =                       *)
(*     Blist.fold_left (fun acc x ->               *)
(*       Blist.fold_left (fun acc y ->             *)
(*         ( x::y ) :: acc)                       *)
(*         acc ys)                                *)
(*       [] xs                                    *)
(*   in                                           *)
(*   let rec choose_ acc = function               *)
(*     | [] -> acc                                *)
(*     | hd::tl -> choose_ (choose_one hd acc) tl *)
(*   in                                           *)
(*   Blist.map Blist.rev (choose_ [[]] l)           *)

(* this should be tail recursive *)
let choose lol =
  let _,lol =
    Blist.fold_left
      (fun (r,a) l -> not r, (if r then Blist.rev l else l)::a)
      (true,[]) lol in
  Blist.fold_left
    (fun ll -> Blist.fold_left (fun tl e -> Blist.fold_left (fun t l -> (e::l)::t) tl ll) [])
    [[]] lol


let rec fixpoint eq f x =
  let fx = f x in
  if eq x fx then x else fixpoint eq f fx

let rec find_first f = function
  | [] -> None
  | h::t -> match f h with
    | None -> find_first f t
    | x -> x

let rec find_some p = function
  | [] -> None
  | x::xs -> if p x then Some x else find_some p xs

(* exception CombinationFound *)

(* let choose_sat_comb =                                                       *)
(*   let rec choose_sat_comb_ f n l comb =                                     *)
(*     if n=0 then                                                             *)
(*       if f comb then raise CombinationFound else ()                         *)
(*     else                                                                    *)
(*       Blist.iter (choose_sat_comb_ f (n-1) l) (Blist.map (fun y-> y::comb) l) *)
(*   in                                                                        *)
(*   fun f n l ->                                                              *)
(*   try                                                                       *)
(*     choose_sat_comb_ f n l [] ; false                                       *)
(*   with CombinationFound -> true                                             *)

let rec replace_nth y n = function
	| [] -> invalid_arg "replace_nth"
	| x::xs -> match n with
    | 0 -> y::xs
    | _ -> x::(replace_nth y (n-1) xs)

let index p l =
  let rec aux p n = function
    | [] -> raise Not_found
    | x::xs -> match p x with
			| true  -> n
    	| false -> aux p (n+1) xs in
  aux p 0 l

let rec indexes p l =
  let rec aux p n = function
    | [] -> []
    | x::xs -> let r = aux p (n+1) xs in if p x then n::r else r in
  aux p 0 l

module Option =
  struct
    let is_none = function None -> true | Some _ -> false
    let is_some = function Some _ -> true | None -> false

    let pred p x =
      if p x then Some x else None

    let get = function
      | Some(x) -> x
      | None -> invalid_arg "Option.get"

    let map f = function
      | None -> None
      | Some x -> f x

    let list_get l = Blist.rev_map get (rev_filter is_some l)
  end

let unzip3 l =
  let rec unzip3acc a1 a2 a3 = function
    | [] -> (Blist.rev a1, Blist.rev a2, Blist.rev a3)
    | (e1, e2, e3)::tail -> unzip3acc (e1::a1) (e2::a2) (e3::a3) tail
  in unzip3acc [] [] [] l

let surround l s r = l ^ s ^ r
let bracket s = surround "(" s ")"
let sqbracket s = surround "[" s "]"
let latex_bracket = bracket
let latex_sqbracket = sqbracket

(*module Funq =                                                          *)
(*  struct                                                               *)
(*    type 'a t = 'a list * 'a list                                      *)
(*                                                                       *)
(*    let make front back = match front with                             *)
(*      | [] -> (Blist.rev back, [])                                      *)
(*      | _  -> (front, back)                                            *)
(*                                                                       *)
(*    let put el (front, back) = make front (el::back)                   *)
(*                                                                       *)
(*    let hd (front, _) = match front with                               *)
(*      | []        -> raise Not_found                                   *)
(*      | el::front -> el                                                *)
(*                                                                       *)
(*    let tl (front, back) = match front with                            *)
(*      | []        -> raise Not_found                                   *)
(*      | el::front -> make front back                                   *)
(*                                                                       *)
(*    let take fq = (hd fq, tl fq)                                       *)
(*                                                                       *)
(*    let put_front el (front, back) = (el::front, back)                 *)
(*                                                                       *)
(*    let is_empty (front, _) = match front with                         *)
(*      | [] -> true                                                     *)
(*      | _  -> false                                                    *)
(*                                                                       *)
(*    let length (front, back) = (Blist.length front) + (Blist.length back)*)
(*    let singleton el = put el ([], [])                                 *)
(*    let empty = ([], [])                                               *)
(*  end                                                                  *)

module Zlist =
  struct
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

    let cons h t = Lazy.lazy_from_val (Node(h, t))

    let rec of_list l = lazy (
      match l with
      | [] -> Empty
      | hd::tl -> Node(hd, of_list tl)
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
      | Node(x,l') -> x::(to_list l')

    let rec find_some f l = match Lazy.force l with
      | Empty -> None
      | Node(h,l') -> match f h with
        | None -> find_some f l'
        | x -> x

    let rec from_while f =
      lazy (
        match f () with
          | None  -> Empty
          | Some x -> Node (x, from_while f)
      )

    (* let append (l1 : 'a t) (l2 : 'a t) =                   *)
    (*   let rec aux list =  match Lazy.force list with       *)
    (*       | Node (x, (t : 'a t)) -> Node (x, lazy (aux t)) *)
    (*       | _                    -> Lazy.force l2          *)
    (*   in lazy (aux l1)                                     *)

    let rec flatten (l : 'a t t) : 'a t =
      lazy (
        match Lazy.force l with
          | Empty -> Empty
          | Node(m, l') -> match Lazy.force m with
            | Empty -> Lazy.force (flatten l')
            | Node(el, m') ->
              Node(el, flatten (Lazy.lazy_from_val (Node(m',l'))))
      )

    (* let choose = function                                                  *)
    (*   | [] -> [[]]                                                         *)
    (*   | hd::tl -> let c = choose tl in                                     *)
    (*     Blist.flatten (Blist.map (fun el -> Blist.map (fun l -> el::l) c) hd) *)

    let rec choose (l : 'a t t) : 'a t t = lazy (
      match Lazy.force l with
        | Empty -> Node(empty, empty)
        | Node(m, tl) ->
          let c = choose tl in
          Lazy.force (flatten (map (fun el -> map (cons el) c) m))
      )

    let find_first p (l : 'a t) = find_some (Option.pred p) l

		let rec fold f ys a = match Lazy.force ys with
			| Empty -> a
			| Node(x, xs) -> fold f xs (f a x)

    let get_opt l = map Option.get (filter Option.is_some l)
  end

(* module Zlist =                          *)
(*   struct                                *)
(*     include Blist                        *)
(*     type 'a t = 'a list                 *)
(*     let empty = []                      *)
(*     let of_list l = l                   *)
(*     let to_list l = l                   *)
(*     let is_empty l = (l=[])             *)
(*     let rec find_some f = function      *)
(*       | [] -> None                      *)
(*       | hd::tl -> match f hd with       *)
(*         | None -> find_some f tl        *)
(*         | x -> x                        *)
(*     let find_first p l = find_first p l *)
(*     let from_hd = function              *)
(*       | [] -> []                        *)
(*       | hd::_ -> [hd]                   *)
(*     let from_fun f = f ()               *)
(*   end                                   *)

(* module type PRIOQ =                                           *)
(*   sig                                                         *)
(*     type 'a left_heap                                         *)

(*     val mk_empty : ('a -> 'a -> int) -> ('a left_heap)        *)
(*     val put : 'a -> 'a left_heap -> 'a left_heap              *)
(*     val take : 'a left_heap -> ('a * 'a left_heap)            *)
(*     val hd : 'a left_heap -> 'a                               *)
(*     val tl : 'a left_heap -> 'a left_heap                     *)
(*     val is_empty : 'a left_heap -> bool                       *)
(*     val singleton : ('a -> 'a -> int) -> 'a -> ('a left_heap) *)
(*     val to_list : 'a left_heap -> 'a list                     *)
(*   end                                                         *)

(* module Prioq : PRIOQ =                                        *)
(*   struct                                                      *)
(*     type 'a heap = E | T of int * 'a * 'a heap * 'a heap      *)
(*     type 'a left_heap = ('a -> 'a -> int) * ('a heap)         *)
(*                                                               *)
(*     let rank = function E -> 0 | T (r,_,_,_) -> r             *)
(*                                                               *)
(*     let makeT x a b =                                         *)
(*       if rank a >= rank b then T (rank b + 1, x, a, b)        *)
(*       else T (rank a + 1, x, b, a)                            *)
(*                                                               *)
(*     let rec merge cmp h1 h2 = match h1, h2 with               *)
(*       | _, E -> h1                                            *)
(*       | E, _ -> h2                                            *)
(*       | T (_, x, a1, b1), T (_, y, a2, b2) ->                 *)
(*         if (cmp x y)<0 then makeT x a1 (merge cmp b1 h2)      *)
(*         else makeT y a2 (merge cmp h1 b2)                     *)
(*                                                               *)
(*     let insert cmp x h = merge cmp (T (1, x, E, E)) h         *)
(*                                                               *)
(*     let find_min = function                                   *)
(*       | E -> invalid_arg "find_min"                           *)
(*       | T (_, x, _, _) -> x                                   *)
(*                                                               *)
(*     let delete_min cmp = function                             *)
(*       | E -> invalid_arg "delete_min"                         *)
(*       | T (_, x, a, b) -> merge cmp a b                       *)
(*                                                               *)
(*     let mk_empty cmp = (cmp, E)                               *)
(*     let put el (cmp, h) = (cmp, insert cmp el h)              *)
(*     let hd (_, h) = find_min h                                *)
(*     let tl (cmp, h) = (cmp, delete_min cmp h)                 *)
(*     let take lh = (hd lh, tl lh)                              *)
(*     let is_empty (_, h) = match h with                        *)
(*       | E -> true                                             *)
(*       | _ -> false                                            *)
(*     let singleton cmp el = put el (mk_empty cmp)              *)
(*                                                               *)
(*     let rec to_list pq =                                      *)
(*       if is_empty pq then [] else                             *)
(*       let hd, tl = take pq in                                 *)
(*       hd :: (to_list tl)                                      *)
(*   end                                                         *)

exception Timeout
let sigalrm_handler = Sys.Signal_handle (fun _ -> raise Timeout)
let w_timeout f timeout =
  let old_behavior = Sys.signal Sys.sigalrm sigalrm_handler in
  let reset_sigalrm () = Sys.set_signal Sys.sigalrm old_behavior in
  if timeout > 0 then ignore (Unix.alarm timeout) ;
  try
    let res = f () in reset_sigalrm () ; Some res
  with Timeout -> (reset_sigalrm () ; None)

module Fun =
	struct
		let neg f x = not (f x)
		let conj f g x = (f x) && (g x)
		let disj f g x = (f x) || (g x)

		let id x = x
		let uncurry f x y = f (x,y)
		let curry f (x,y) = f x y
		let swap f x y = f y x
  end