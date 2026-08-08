module Make (T : Utilsigs.BasicType) : sig
  type elt = T.t

  val length : 'a list -> int
  val compare_lengths : 'a list -> 'b list -> int
  val compare_length_with : 'a list -> int -> int
  val cons : 'a -> 'a list -> 'a list
  val hd : 'a list -> 'a
  val tl : 'a list -> 'a list
  val nth : 'a list -> int -> 'a
  val nth_opt : 'a list -> int -> 'a option
  val rev : 'a list -> 'a list
  val init : int -> (int -> 'a) -> 'a list
  val append : 'a list -> 'a list -> 'a list
  val rev_append : 'a list -> 'a list -> 'a list
  val concat : 'a list list -> 'a list
  val flatten : 'a list list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val iteri : (int -> 'a -> unit) -> 'a list -> unit
  val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
  val rev_map : ('a -> 'b) -> 'a list -> 'b list
  val filter_map : ('a -> 'b option) -> 'a list -> 'b list
  val concat_map : ('a -> 'b list) -> 'a list -> 'b list

  val fold_left_map :
    ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a list -> 'acc * 'b list

  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc
  val fold_right : ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc
  val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
  val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list

  val fold_left2 :
    ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a list -> 'b list -> 'acc

  val fold_right2 :
    ('a -> 'b -> 'acc -> 'acc) -> 'a list -> 'b list -> 'acc -> 'acc

  val for_all : ('a -> bool) -> 'a list -> bool
  val exists : ('a -> bool) -> 'a list -> bool
  val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
  val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
  val memq : 'a -> 'a list -> bool
  val find_mapi : (int -> 'a -> 'b option) -> 'a list -> 'b option
  val filter : ('a -> bool) -> 'a list -> 'a list
  val find_all : ('a -> bool) -> 'a list -> 'a list
  val filteri : (int -> 'a -> bool) -> 'a list -> 'a list
  val take : int -> 'a list -> 'a list
  val drop : int -> 'a list -> 'a list
  val take_while : ('a -> bool) -> 'a list -> 'a list
  val drop_while : ('a -> bool) -> 'a list -> 'a list
  val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
  val partition_map : ('a -> ('b, 'c) Either.t) -> 'a list -> 'b list * 'c list
  val assoc : 'a -> ('a * 'b) list -> 'b
  val assoc_opt : 'a -> ('a * 'b) list -> 'b option
  val assq : 'a -> ('a * 'b) list -> 'b
  val assq_opt : 'a -> ('a * 'b) list -> 'b option
  val mem_assoc : 'a -> ('a * 'b) list -> bool
  val mem_assq : 'a -> ('a * 'b) list -> bool
  val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
  val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
  val combine : 'a list -> 'b list -> ('a * 'b) list
  val sort : ('a -> 'a -> int) -> 'a list -> 'a list
  val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
  val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
  val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list
  val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
  val to_seq : 'a list -> 'a Seq.t
  val of_seq : 'a Seq.t -> 'a list
  val empty : 'a list
  val is_empty : 'a list -> bool
  val singleton : 'a -> 'a list
  val to_list : 'a list -> 'a list
  val decons : 'a list -> 'a * 'a list
  val foldl : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
  val foldr : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val bind : ('a -> 'b list) -> 'a list -> 'b list
  val rev_filter : ('a -> bool) -> 'a list -> 'a list
  val but_last : 'a list -> 'a list
  val remove_nth : int -> 'a list -> 'a list
  val replace_nth : 'a -> int -> 'a list -> 'a list
  val repeat : 'a -> int -> 'a list
  val range : int -> 'a list -> int list
  val indexes : 'a list -> int list
  val find_index : ('a -> bool) -> 'a list -> int
  val find_indexes : ('a -> bool) -> 'a list -> int list
  val uniq : ('a -> 'a -> bool) -> 'a list -> 'a list
  val cartesian_product : 'a list -> 'b list -> ('a * 'b) list
  val cartesian_hemi_square : 'a list -> ('a * 'a) list
  val pairs : 'a list -> ('a * 'a) list

  type t = elt list

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val of_list : elt list -> elt list
  val min_elt : 'a list -> 'a
  val elements : 'a list -> 'a list
  val add : elt -> elt list -> elt list
  val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val cardinal : 'a list -> int
  val choose : 'a list -> 'a
  val union : elt list -> elt list -> elt list
  val union_of_list : elt list list -> elt list
  val map : ('a -> elt) -> 'a list -> elt list
  val mem : elt -> elt list -> bool
  val max_elt : 'a list -> 'a
  val del_first : ('a -> bool) -> 'a list -> 'a list
  val remove : elt -> elt list -> elt list
  val inter : elt list -> elt list -> elt list
  val subset : elt list -> elt list -> bool
  val diff : elt list -> elt list -> elt list
  val split : elt -> elt list -> elt list * bool * elt list
  val map_to : ('a -> 'b -> 'b) -> 'b -> ('c -> 'a) -> 'c list -> 'b
  val opt_map_to : ('a -> 'b -> 'b) -> 'b -> ('c -> 'a option) -> 'c list -> 'b
  val map_to_list : ('a -> 'b) -> 'a list -> 'b list
  val to_rev_seq : 'a list -> 'a Seq.t

  val weave :
    ('a -> 'b -> 'b list) ->
    ('a -> 'b -> 'c) ->
    ('c list -> 'c) ->
    'a list ->
    'b ->
    'c

  val find_suchthat : ('a -> bool) -> 'a list -> 'a
  val find_suchthat_opt : ('a -> bool) -> 'a list -> 'a option
  val find_opt : elt -> elt list -> elt option
  val find : elt -> elt list -> elt
  val find_map : ('a -> 'b option) -> 'a list -> 'b option
  val count : ('a -> bool) -> 'a list -> int
  val subsets : elt list -> elt list list
  val disjoint : elt list -> elt list -> bool
  val find_last_opt : 'a -> 'b -> 'c
  val find_last : 'a -> 'b -> 'c
  val find_first_opt : 'a -> 'b -> 'c
  val find_first : 'a -> 'b -> 'c
  val choose_opt : 'a -> 'b
  val max_elt_opt : 'a -> 'b
  val min_elt_opt : 'a -> 'b
  val add_seq : 'a -> 'b -> 'c
  val to_seq_from : 'a -> 'b -> 'c

  val mk_unifier :
    bool ->
    bool ->
    (elt -> elt -> ('a -> 'b option) -> 'a -> 'b option) ->
    Flist.Make(T).t ->
    Flist.Make(T).t ->
    ('a -> 'b option) ->
    'a ->
    'b option
end
