val is_none : 'a option -> bool
val is_some : 'a option -> bool
val pred : ('a -> bool) -> 'a -> 'a option
val mk : bool -> 'a -> 'a option
val mk_lazily : bool -> (unit -> 'a) -> 'a option
val get : 'a option -> 'a
val map : ('a -> 'b) -> 'a option -> 'b option
val list_get : 'a option list -> 'a list
val some : 'a -> 'a option
val dest : 'a -> ('b -> 'a) -> 'b option -> 'a
val dest_lazily : (unit -> 'a) -> ('b -> 'a) -> 'b option -> 'a
val pred_dest : ('a -> bool) -> 'a option -> bool
val flatten : 'a option option -> 'a option
val bind : ('a -> 'b option) -> 'a option -> 'b option

val pp :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit

val iter : ('a -> unit) -> 'a option -> unit
val fold : ('a -> 'b -> 'b) -> 'a option -> 'b -> 'b
