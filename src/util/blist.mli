(** List operations. The [List] standard library module is included here. *)

(** The type of a t. Used for modularity purposed (e.g., substituting
the whole module with a lazy t implementation. *)
type 'a t = 'a list

val length : 'a t -> int
(** Return the length (number of elements) of the given t. *)

val hd : 'a t -> 'a
(** Return the first element of the given t. Raise
   [Failure "hd"] if the t is empty. *)

val tl : 'a t -> 'a t
(** Return the given t without its first element. Raise
   [Failure "tl"] if the t is empty. *)

val nth : 'a t -> int -> 'a
(** Return the [n]-th element of the given t.
   The first element (head of the t) is at position 0.
   Raise [Failure "nth"] if the t is too short.
   Raise [Invalid_argument "List.nth"] if [n] is negative. *)

val rev : 'a t -> 'a t
(** List reversal. *)

val append : 'a t -> 'a t -> 'a t
(** Catenate two ts.  Same function as the infix operator [@].
   Not tail-recursive (length of the first argument).  The [@]
   operator is not tail-recursive either. *)

val rev_append : 'a t -> 'a t -> 'a t
(** [List.rev_append l1 l2] reverses [l1] and concatenates it to [l2].
   This is equivalent to {!List.rev}[ l1 @ l2], but [rev_append] is
   tail-recursive and more efficient. *)

val concat : 'a t t -> 'a t
(** Concatenate a t of ts.  The elements of the argument are all
   concatenated together (in the same order) to give the result.
   Not tail-recursive
   (length of the argument + length of the longest sub-list). *)

val flatten : 'a t t -> 'a t
(** Same as [concat].  Not tail-recursive
   (length of the argument + length of the longest sub-list). *)

(** {6 Iterators} *)

val iter : ('a -> unit) -> 'a t -> unit
(** [List.iter f [a1; ...; an]] applies function [f] in turn to
   [a1; ...; an]. It is equivalent to
   [begin f a1; f a2; ...; f an; () end]. *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** Same as {!List.iter}, but the function is applied to the index of
   the element as first argument (counting from 0), and the element
   itself as second argument.
   @since 4.00.0
*)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [List.map f [a1; ...; an]] applies function [f] to [a1, ..., an],
   and builds the t [[f a1; ...; f an]]
   with the results returned by [f].  Not tail-recursive. *)

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** Same as {!List.map}, but the function is applied to the index of
   the element as first argument (counting from 0), and the element
   itself as second argument.  Not tail-recursive.
   @since 4.00.0
*)

val rev_map : ('a -> 'b) -> 'a t -> 'b t
(** [List.rev_map f l] gives the same result as
   {!List.rev}[ (]{!List.map}[ f l)], but is tail-recursive and
   more efficient. *)

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [List.fold_left f a [b1; ...; bn]] is
   [f (... (f (f a b1) b2) ...) bn]. *)

val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** [List.fold_right f [a1; ...; an] b] is
   [f a1 (f a2 (... (f an b) ...))].  Not tail-recursive. *)

(** {6 Iterators on two ts} *)

val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** [List.iter2 f [a1; ...; an] [b1; ...; bn]] calls in turn
   [f a1 b1; ...; f an bn].
   Raise [Invalid_argument] if the two ts have
   different lengths. *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [List.map2 f [a1; ...; an] [b1; ...; bn]] is
   [[f a1 b1; ...; f an bn]].
   Raise [Invalid_argument] if the two ts have
   different lengths.  Not tail-recursive. *)

val rev_map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [List.rev_map2 f l1 l2] gives the same result as
   {!List.rev}[ (]{!List.map2}[ f l1 l2)], but is tail-recursive and
   more efficient. *)

val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b t -> 'c t -> 'a
(** [List.fold_left2 f a [b1; ...; bn] [c1; ...; cn]] is
   [f (... (f (f a b1 c1) b2 c2) ...) bn cn].
   Raise [Invalid_argument] if the two ts have
   different lengths. *)

val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
(** [List.fold_right2 f [a1; ...; an] [b1; ...; bn] c] is
   [f a1 b1 (f a2 b2 (... (f an bn c) ...))].
   Raise [Invalid_argument] if the two ts have
   different lengths.  Not tail-recursive. *)

(** {6 List scanning} *)

val for_all : ('a -> bool) -> 'a t -> bool
(** [for_all p [a1; ...; an]] checks if all elements of the t
   satisfy the predicate [p]. That is, it returns
   [(p a1) && (p a2) && ... && (p an)]. *)

val exists : ('a -> bool) -> 'a t -> bool
(** [exists p [a1; ...; an]] checks if at least one element of
   the t satisfies the predicate [p]. That is, it returns
   [(p a1) || (p a2) || ... || (p an)]. *)

val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** Same as {!List.for_all}, but for a two-argument predicate.
   Raise [Invalid_argument] if the two ts have
   different lengths. *)

val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** Same as {!List.exists}, but for a two-argument predicate.
   Raise [Invalid_argument] if the two ts have
   different lengths. *)

val mem : 'a -> 'a t -> bool
(** [mem a l] is true if and only if [a] is equal
   to an element of [l]. *)

val memq : 'a -> 'a t -> bool
(** Same as {!List.mem}, but uses physical equality instead of structural
   equality to compare t elements. *)

(** {6 List searching} *)

val find : ('a -> bool) -> 'a t -> 'a
(** [find p l] returns the first element of the t [l]
   that satisfies the predicate [p].
   Raise [Not_found] if there is no value that satisfies [p] in the
   t [l]. *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** [filter p l] returns all the elements of the t [l]
   that satisfy the predicate [p].  The order of the elements
   in the input t is preserved.  *)

val find_all : ('a -> bool) -> 'a t -> 'a t
(** [find_all] is another name for {!List.filter}. *)

val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
(** [partition p l] returns a pair of ts [(l1, l2)], where
   [l1] is the t of all the elements of [l] that
   satisfy the predicate [p], and [l2] is the t of all the
   elements of [l] that do not satisfy [p].
   The order of the elements in the input t is preserved. *)

(** {6 Association ts} *)

val assoc : 'a -> ('a * 'b) t -> 'b
(** [assoc a l] returns the value associated with key [a] in the t of
   pairs [l]. That is,
   [assoc a [ ...; (a,b); ...] = b]
   if [(a,b)] is the leftmost binding of [a] in t [l].
   Raise [Not_found] if there is no value associated with [a] in the
   t [l]. *)

val assq : 'a -> ('a * 'b) t -> 'b
(** Same as {!List.assoc}, but uses physical equality instead of structural
   equality to compare keys. *)

val mem_assoc : 'a -> ('a * 'b) t -> bool
(** Same as {!List.assoc}, but simply return true if a binding exists,
   and false if no bindings exist for the given key. *)

val mem_assq : 'a -> ('a * 'b) t -> bool
(** Same as {!List.mem_assoc}, but uses physical equality instead of
   structural equality to compare keys. *)

val remove_assoc : 'a -> ('a * 'b) t -> ('a * 'b) t
(** [remove_assoc a l] returns the t of
   pairs [l] without the first pair with key [a], if any.
   Not tail-recursive. *)

val remove_assq : 'a -> ('a * 'b) t -> ('a * 'b) t
(** Same as {!List.remove_assoc}, but uses physical equality instead
   of structural equality to compare keys.  Not tail-recursive. *)

(** {6 Lists of pairs} *)

val split : ('a * 'b) t -> 'a t * 'b t
(** Transform a t of pairs into a pair of ts:
   [split [(a1,b1); ...; (an,bn)]] is [([a1; ...; an], [b1; ...; bn])].
   Not tail-recursive.
*)

val combine : 'a t -> 'b t -> ('a * 'b) t
(** Transform a pair of ts into a t of pairs:
   [combine [a1; ...; an] [b1; ...; bn]] is
   [[(a1,b1); ...; (an,bn)]].
   Raise [Invalid_argument] if the two ts
   have different lengths.  Not tail-recursive. *)

(** {6 Sorting} *)

val sort : ('a -> 'a -> int) -> 'a t -> 'a t
(** Sort a t in increasing order according to a comparison
   function.  The comparison function must return 0 if its arguments
   compare as equal, a positive integer if the first is greater,
   and a negative integer if the first is smaller (see Array.sort for
   a complete specification).  For example,
   {!Pervasives.compare} is a suitable comparison function.
   The resulting t is sorted in increasing order.
   [List.sort] is guaranteed to run in constant heap space
   (in addition to the size of the result t) and logarithmic
   stack space.

   The current implementation uses Merge Sort. It runs in constant
   heap space and logarithmic stack space.
*)

val stable_sort : ('a -> 'a -> int) -> 'a t -> 'a t
(** Same as {!List.sort}, but the sorting algorithm is guaranteed to
   be stable (i.e. elements that compare equal are kept in their
   original order) .

   The current implementation uses Merge Sort. It runs in constant
   heap space and logarithmic stack space.
*)

val fast_sort : ('a -> 'a -> int) -> 'a t -> 'a t
(** Same as {!List.sort} or {!List.stable_sort}, whichever is faster
    on typical input. *)

val sort_uniq : ('a -> 'a -> int) -> 'a t -> 'a t
(** Same as {!List.sort}, but also remove duplicates.
    @since 4.02.0 *)

val merge : ('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
(** Merge two ts:
    Assuming that [l1] and [l2] are sorted according to the
    comparison function [cmp], [merge cmp l1 l2] will return a
    sorted t containting all the elements of [l1] and [l2].
    If several elements compare equal, the elements of [l1] will be
    before the elements of [l2].
    Not tail-recursive (sum of the lengths of the arguments).
*)

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

(** {6 Searching lists} *)

val find_opt : ('a -> bool) -> 'a t -> 'a option
(** [find_opt pred l] returns [Some x] for the first [x] in [l] such that [pred x = true], or [None]. *)

val find_map : ('a -> 'b option) -> 'a t -> 'b option
(** Optimisation for finding and converting at the same time. [find_map f l]
will return [f x] for the first [x] in [l] such that [f x] is not [None], or [None]. *)

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
