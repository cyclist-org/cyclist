(** Combinators for manipulating functions. *)

type 'a predicate = 'a -> bool

val _true : 'a -> bool
val _false : 'a -> bool
val neg : ('a -> bool) -> 'a -> bool
val conj : ('a -> bool) -> ('a -> bool) -> 'a -> bool
val list_conj : ('a -> bool) list -> 'a -> bool
val disj : ('a -> bool) -> ('a -> bool) -> 'a -> bool
val list_disj : ('a -> bool) list -> 'a -> bool
val id : 'a -> 'a
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val swap : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val direct : bool -> ('a -> 'a -> 'b) -> 'a -> 'a -> 'b
val iter : ('a -> 'a) -> int -> 'a -> 'a
