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

val prefixes : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** [prefixes ~eq xs ys] returns [true] if and only if the first [length xs]
    elements of [xs] and [ys] are pairwise "equal", according to [eq].
    When the optional argument [eq] is omitted, then [Stdlib.( = )] is used. *)

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

val find_map_or : ('a -> 'b) -> ('b -> bool) -> 'a list -> ('b, 'b list) Either.t
(** [find_map_or map_f pred xs] returns [Left (map_f x)] where [x] is the first
    element of [xs] such that [pred (map_f x)] returns [true] or else returns
    [Right ys], where [ys] is equivalent to the result of [map map_f xs], when
    no such [x] exists.
    In case a result of the form [Left (map_f x)] is returned, [map_f] is only
    applied to the elements up to and including [x]. *)

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
    special way, producing only a single value using the [tie] function.
*)

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

val sublist_index : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> int option
(** [sublist ~eq xs ys] returns [Some i] when [i] is the smallest index such
    that [xs] is a prefix of [drop i ys], where elements [x] and [y] are
    considered equal when [eq x y] returns [true].
    This will always be in the range 0 <= [i] < [length ys].
    Raises [None] when no such index exists. *)

val sublist_last_index : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> int option
(** [sublist ~eq xs ys] returns [Some i] when [i] is the largest index such
    that [xs] is a prefix of [drop i ys], where elements [x] and [y] are
    considered equal when [eq x y] returns [true].
    This will always be in the range 0 <= [i] <= [length ys],
    and [i] == [length ys] only when [xs] is the empty list.
    Raises [None] when no such index exists. *)

(** {6 List manipulation and conversion} *)

val del_first : ('a -> bool) -> 'a t -> 'a t
(** Delete first element satisfying a given predicate. *)

val uniq : ('a -> 'a -> bool) -> 'a t -> 'a t
(** [uniq eq l] returns a list containing no duplicates w.r.t. element equality [eq]. *)

val intersperse : 'a -> 'a t -> 'a t
(** Insert given element between elements of given list. *)

val interleave : 'a t -> 'a t -> 'a t
(** [interleave xs ys] interleave the elements of [xs] and [ys].
    E.g. if [xs = [a;b;c]] and [ys = [d;e;f]], then the result is the list
    [[a;d;b;e;c;f]].
    If [xs] and [ys] have different lengths, the result is the interleaving of
    of the truncation of the lists to the shortest length, followed by the
    remainder of the longer list.
    E.g. if [xs = [a;b;c]] and [ys = [d;e;f;g;h]], then the result is the list
    [[a;d;b;e;c;f;g;h]]. *)

val unzip3 : ('a * 'b * 'c) t -> 'a t * 'b t * 'c t

val zip3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

(** {6 Searching lists} *)

(* This exists in OCaml's List module only from version 4.10 *)
val find_map : ('a -> 'b option) -> 'a t -> 'b option
(** Optimisation for finding and converting at the same time. [find_map f l]
will return [f x] for the first [x] in [l] such that [f x] is not [None], or [None]. *)

(** {6 Combinatorial functions } *)

val cartesian_product : 'a t -> 'b t -> ('a * 'b) t

val cartesian_map : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

val cartesian_hemi_square : 'a t -> ('a * 'a) t
(** Return a list of all pairs out of elements of a list, but without including
symmetric pairs.  Useful for symmetric relations. *)

val pairs : 'a t -> ('a * 'a) t
(** Return a list of pairs of consecutive elements. *)

val choose : 'a t t -> 'a t t
(** [choose [[1;2;3]; [4;5]]] returns [[[1;4];[1;5];[2;4];[2;5];[3;4];[3;5]]]. *)

val combs : int -> 'a t -> 'a t t
(** [combs n l] returns all combinations of [n] elements from [l]. *)

val all_combs : ?include_empty:bool -> 'a t -> 'a t t
(** [all ~include_empty combs l] returns all ways of choosing [n] elements from
    [l] for all [n] < [length l]. When [include_empty] is [true], this also
    includes the empty list (i.e. the case when [n] is zero).
    The default value for [include_empty] is [false]. *)

val all_splits : ?include_empty:bool -> 'a t -> ('a t * 'a t) t
(** [all_partitions ~include_empty xs] returns a list of all possible ways of
    splitting [xs] into two lists; i.e. the result contains all pairs
    [(take i xs, drop i xs)] for 0 < [i] < [length xs].
    If [include_empty] is [true], or omitted, then the result contains all such
    pairs for 0 <= [i] <= [length xs].
    This is functionally equivalent to
      [if allow_empty
         then init ((length xs) + 1) (fun i -> (take i xs, drop i xs))
         else init ((length xs) - 1) (fun i -> (take (i+1) xs, drop (i+1) xs))]
    but is more efficient. Tail-recursive. *)

val longest_common_prefix : ?eq:('a -> 'a -> bool) -> 'a t t -> 'a t
(** [longest_common_prefix ?eq xss] returns the largest prefix of the first
    element [xs] of [xss] such that each element of [xs] is "equal" to the
    element at the corresponding position in each other element of [xss],
    according to the predicate [eq].
    When the optional argument [eq] is omitted, then [Stdlib.( = )] is used. *)

val longest_common_suffix : ?eq:('a -> 'a -> bool) -> 'a t t -> 'a t
(** [longest_common_suffix ?eq xss] returns the largest suffix of the first
    element [xs] of [xss] such that each element of [xs] is "equal" to the
    element at the corresponding position (from the end) in each other element
    of [xss], according to the predicate [eq].
    When the optional argument [eq] is omitted, then [Stdlib.( = )] is used. *)
