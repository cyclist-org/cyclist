(** Signatures for containers and essential types. *)

module type BasicType =
  sig
    type t
    val compare : t -> t -> int
    (** Standard comparator, return <0 if first less than second, 0 if equal, >0 if greater. *)
    val equal : t -> t -> bool
    (** Standard equality predicate. *)
    val hash : t -> int
    (** Standard hash function. *)
    val to_string : t -> string
    (** Convert to string. *)
    val pp : Format.formatter -> t -> unit
    (** Pretty printer. *)
  end
(** Most types for use in containers, maps and other stuff must provide the
    above essential methods. *)

module type OrderedContainer =
  sig
    type t
    include BasicType with type t:=t
    include Set.S with type t:=t
    val of_list : elt list -> t
    (** Convert a list of elements to a container. *)
    val to_list: t -> elt list
    (** Convert to a list of unique, sorted elements. *)
    val endomap : (elt -> elt) -> t -> t
    (** Map a set of elements to another set of elements of the same type. 
        The size of the set may reduce if there are duplicates produced. *)
    val map_to : ('b -> 'a -> 'a) -> 'a -> (elt -> 'b) -> t -> 'a
    (** [map_to add empty f set] converts every element of [set] using [f], 
        and then folds over the new collection of elements using as starting
        value [empty] and folding operation [add]. *)
    val map_to_list : (elt -> 'a) -> t -> 'a list
    (** [map_to_list f set] applies [f] to every element and constructs a list
        of results.  The list is ordered just like the container. *)
    val weave : (elt -> 'a -> 'a list) -> (elt -> 'a -> 'b) -> ('b list -> 'b) -> t -> 'a -> 'b
    (** Weave combinator - used in the SL Model Checker. 
        [weave split tie join xs acc]
        is a generalised form of a fold - it takes as arguments three    
        operations ([split], [tie], and [join]), a container [xs] to weave (i.e. fold) over, 
        and an accumulator [acc]. Whereas a fold combines the previously accumulated     
        value with the next value in the set to produce the new accumulated       
        value, a weave uses its [split] argument to combine the next element in    
        the set with the previously accumulated value to produce a *list* of new  
        accumulated values - not just a single new value. Each new value in this   
        list is then used as the accumulator for a distinct recursive call to the  
        weave function - compared with a single recursive call for a fold. Thus,   
        at this point, the weave produces a list of final values, which are then   
        combined using the [join] function argument. Furthermore, in constrast to  
        fold, the weave combinator treats the final element in the list in a       
        special way, producing only a single value using the [tie] function. *)
    val find : (elt -> bool) -> t -> elt
    (** [find p set] returns the first element of [set]
        that satisfies the predicate [p].
        Raise [Not_found] if there is no value that satisfies [p] in [set]. *)
    val find_opt : (elt -> bool) -> t -> elt option
    (** [find_opt pred set] returns [Some x] for the first [x] in [set] such 
        that [pred x = true], or [None]. *)
    val find_map : (elt -> 'a option) -> t -> 'a option
    (** Optimisation for finding and converting at the same time. [find_map f set]
        will return [f x] for the first [x] in [set] such that [f x] is not [None], 
        or [None] otherwise. *)
    val union_of_list : t list -> t
    (** Union a list of sets. *)
    val subsets : t -> t list
    (** Generate all subsets of a set. *)
    val fixpoint : (t -> t) -> t -> t
    val del_first : (elt -> bool) -> t -> t
    (** Remove first element satisfying the given predicate. *)
    val disjoint : t -> t -> bool
    (** Decide if there are no common elements. *)
  end
(** A (persistent) ordered container, generalising the standard [Set] container. *)

module type OrderedMap =
  sig
    include Map.S
    val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
    (** [pp pp_val fmt map] pretty prints [map] using [pp_val] to pretty-print
        the *values* of [map].  *Keys* are pretty-printed using the [pp] function
        of the [BasicType] for keys. *)
    val to_string : ('a -> string) -> 'a t -> string
    (** [to_string val_to_string map] converts to a string, using [val_to_string]
        to convert *values* to strings. *)
    val hash : ('a -> int) -> 'a t -> int
    (** Hash the map using the provided function for hashing *values*. *)
    val of_list : (key * 'a) list -> 'a t
    (** Create a map out of a list of pairs of (keys, values). *)
    val to_list : 'a t -> (key * 'a) list
    (** Create a list of pairs (keys, values) out of a map. *)
    val endomap : ((key * 'a) -> (key * 'b)) -> 'a t -> 'b t
    (** Convert a map to another, by enumerating bindings in key order, 
        converting them into new ones and adding them to a new map. 
        Bindings created earlier have precedence. *)
    val union : 'a t -> 'a t -> 'a t
    (** Union two maps. Bindings in the first map have precedence. *)
    val find_map : (key -> 'a -> bool) -> 'a t -> (key * 'a) option
    (** Optimisation for finding and converting at the same time. [find_map f map]
        will return [f k v] for the first [k],[v] in [map] such that [f k v] is not [None], 
        or [None] otherwise. *)
    val fixpoint : ('a -> 'a -> bool) -> ('a t -> 'a t) -> 'a t -> 'a t
    (** [fixpoint val_equal f map] computes the fixpoint of [f] using [val_equal]
        to compare *values*. *)
(*    val map_to : ('b -> 'c -> 'c) -> 'c -> (key -> 'v -> 'b) -> 'v t -> 'c *)
(*    val map_to_list : (key -> 'b -> 'a) -> 'b t -> 'a list                 *)
    val submap : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    (** Decide if a map is included in another, using the provided value equality
        predicate. *)
    val add_bindings : (key * 'a) list -> 'a t -> 'a t
    (** Add all bindings in provided list to map.  Bindings already in the map
        have precedence. *)
  end
(** An ordered map, extending the standard [Map] module. *)
