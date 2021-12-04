(** List operations. The [List] standard library module is included here. *)
include module type of List

(** {6 Essential methods} *)

val empty : 'a t
(** The empty list constant. *)

val is_empty : 'a t -> bool
(** Is the argument the empty list? *)

val singleton : 'a -> 'a t
(** Constructs a list with exactly one element, the argument provided. *)

val to_string : string -> ('a -> string) -> 'a t -> string
(** [to_string sep e l] converts the list [l] to a string. [e] is the function
that turns an element into a string and [sep] is the separator appearring between
elements. *)

val pp :
     (Format.formatter -> unit -> unit)
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a t
  -> unit
(** [pp sep e fmt l] pretty prints the list [l]. [e] is the function
that pretty prints an element and [sep] is function that pretty prints a separator. *)

val equal : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** [equal eq l l'] computes pointwise equality between [l] and [l'] assuming
[eq] computes equality between elements.*)

val of_list : 'a list -> 'a t
(** Construct a [t] list out of a primitive list. Just the identity in this module. *)

val to_list : 'a t -> 'a list
(** Construct a primitive list out of a [t] list. Just the identity in this module. *)

val cons : 'a -> 'a t -> 'a t
(** Equivalent to [::]. Will go away in OCaml 4.03. *)

val decons : 'a t -> 'a * 'a t
(** Destruct a non-empty list. *)

(** {6 Combinator helper methods} *)

val foldl : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

val foldr : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val bind : ('a -> 'b t) -> 'a t -> 'b t

val rev_filter : ('a -> bool) -> 'a t -> 'a t

val map_to : ('a -> 'b -> 'b) -> 'b -> ('c -> 'a) -> 'c t -> 'b

val opt_map_to : ('a -> 'b -> 'b) -> 'b -> ('c -> 'a option) -> 'c t -> 'b
(** [opt_map_to oadd oempty f xs] is equivalent to [map_to (Option.dest Fun.id oadd) oempty f x] *)

val weave :
  ('a -> 'b -> 'b t) -> ('a -> 'b -> 'c) -> ('c t -> 'c) -> 'a t -> 'b -> 'c
(** Weave combinator - used in the SL Model Checker. 
A "weave" is a generalised form of a fold - it takes as arguments three    
operations ([split], [tie], and [join]), a list to weave (i.e. fold) over, 
and an accumulator. Whereas a fold combines the previously accumulated     
value with the next value in the list to produce the new accumulated       
value, a weave uses its [split] argument to combine the next element in    
the list with the previously accumulated value to produce a *list* of new  
accumulated values - not just a single new value. Each new value in this   
list is then used as the accumulator for a distinct recursive call to the  
weave function - compared with a single recursive call for a fold. Thus,   
at this point, the weave produces a list of final values, which are then   
combined using the [join] function argument. Furthermore, in constrast to  
fold, the weave combinator treats the final element in the list in a       
special way, producing only a single value using the [tie] function. *)

(** {6 Positional helper methods} *)

val take : int -> 'a t -> 'a t
(** [take n l] returns a list of the first [n] elements of [l]. *)

val drop : int -> 'a t -> 'a t
(** [drop n l] returns the suffix of [l] after skipping [n] elements. *)

val but_last : 'a t -> 'a t
(** Return a list containing all elements apart from the last one. *)

val remove_nth : int -> 'a t -> 'a t

val replace_nth : 'a -> int -> 'a t -> 'a t

val repeat : 'a -> int -> 'a t
(** [repeat e n] constructs a list of length [n] where all elements are physically 
equal to [e]. *)

val range : int -> 'a t -> int t
(** [range n l] returns a list of increasing integers [li] such that [hd li = n]
and [length li = length l]. *)

val indexes : 'a t -> int t
(** [indexes l] returns the list of integer positions of elements in [l]. *)

val find_index : ('a -> bool) -> 'a t -> int
(** [find_index pred l] returns the position of the first [x] in [l] such that [pred x = true] or throws [Not_found]. *)

val find_indexes : ('a -> bool) -> 'a t -> int t
(** [find_indexes pred l] returns the list of positions of all [x] in [l] such that [pred x = true]. *)

(** {6 List manipulation and conversion} *)

val del_first : ('a -> bool) -> 'a t -> 'a t
(** Delete first element satisfying a given predicate. *)

val uniq : ('a -> 'a -> bool) -> 'a t -> 'a t
(** [uniq eq l] returns a list containing no duplicates w.r.t. element equality [eq]. *)

val intersperse : 'a -> 'a t -> 'a t
(** Insert given element between elements of given list. *)

val unzip3 : ('a * 'b * 'c) t -> 'a t * 'b t * 'c t

val zip3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

(** {6 Combinatorial functions } *)

val cartesian_product : 'a t -> 'b t -> ('a * 'b) t

val cartesian_hemi_square : 'a t -> ('a * 'a) t
(** Return a list of all pairs out of elements of a list, but without including
symmetric pairs.  Useful for symmetric relations. *)

val choose : 'a t t -> 'a t t
(** [choose [[1;2;3]; [4;5]]] returns [[[1;4];[1;5];[2;4];[2;5];[3;4];[3;5]]]. *)

val combs : int -> 'a t -> 'a t t
(** [combs n l] returns all combinations of [n] elements from [l]. *)

val pairs : 'a t -> ('a * 'a) t
(** Return a list of pairs of consecutive elements. *)
