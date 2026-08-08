module type BasicType = sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end

module type OrderedContainer = sig
  type t

  val hash : t -> int
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit

  type elt

  val empty : t
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val cardinal : t -> int
  val elements : t -> elt list
  val min_elt : t -> elt
  val min_elt_opt : t -> elt option
  val max_elt : t -> elt
  val max_elt_opt : t -> elt option
  val choose : t -> elt
  val choose_opt : t -> elt option
  val find : elt -> t -> elt
  val find_opt : elt -> t -> elt option
  val find_first : (elt -> bool) -> t -> elt
  val find_first_opt : (elt -> bool) -> t -> elt option
  val find_last : (elt -> bool) -> t -> elt
  val find_last_opt : (elt -> bool) -> t -> elt option
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val map : (elt -> elt) -> t -> t
  val filter : (elt -> bool) -> t -> t
  val filter_map : (elt -> elt option) -> t -> t
  val partition : (elt -> bool) -> t -> t * t
  val split : elt -> t -> t * bool * t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val subset : t -> t -> bool
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val of_list : elt list -> t
  val to_seq_from : elt -> t -> elt Seq.t
  val to_seq : t -> elt Seq.t
  val to_rev_seq : t -> elt Seq.t
  val add_seq : elt Seq.t -> t -> t
  val of_seq : elt Seq.t -> t
  val to_list : t -> elt list
  val map_to : ('b -> 'a -> 'a) -> 'a -> (elt -> 'b) -> t -> 'a
  val opt_map_to : ('b -> 'a -> 'a) -> 'a -> (elt -> 'b option) -> t -> 'a
  val map_to_list : (elt -> 'a) -> t -> 'a list

  val weave :
    (elt -> 'a -> 'a list) ->
    (elt -> 'a -> 'b) ->
    ('b list -> 'b) ->
    t ->
    'a ->
    'b

  val find_suchthat : (elt -> bool) -> t -> elt
  val find_suchthat_opt : (elt -> bool) -> t -> elt option
  val find_map : (elt -> 'a option) -> t -> 'a option
  val count : (elt -> bool) -> t -> int
  val union_of_list : t list -> t
  val subsets : t -> t list
  val fixpoint : (t -> t) -> t -> t
  val del_first : (elt -> bool) -> t -> t
  val disjoint : t -> t -> bool

  val mk_unifier :
    bool ->
    bool ->
    ('a, 'b, elt) Unification.cps_unifier ->
    ('a, 'b, t) Unification.cps_unifier
end

module type OrderedMap = sig
  type key
  type +!'a t

  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val add_to_list : key -> 'a -> 'a list t -> 'a list t
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val singleton : key -> 'a -> 'a t
  val remove : key -> 'a t -> 'a t

  val merge :
    (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t

  val cardinal : 'a t -> int
  val bindings : 'a t -> (key * 'a) list
  val min_binding : 'a t -> key * 'a
  val min_binding_opt : 'a t -> (key * 'a) option
  val max_binding : 'a t -> key * 'a
  val max_binding_opt : 'a t -> (key * 'a) option
  val choose : 'a t -> key * 'a
  val choose_opt : 'a t -> (key * 'a) option
  val find : key -> 'a t -> 'a
  val find_opt : key -> 'a t -> 'a option
  val find_first : (key -> bool) -> 'a t -> key * 'a
  val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
  val find_last : (key -> bool) -> 'a t -> key * 'a
  val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val split : key -> 'a t -> 'a t * 'a option * 'a t
  val is_empty : 'a t -> bool
  val mem : key -> 'a t -> bool
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  val exists : (key -> 'a -> bool) -> 'a t -> bool
  val to_seq : 'a t -> (key * 'a) Seq.t
  val to_rev_seq : 'a t -> (key * 'a) Seq.t
  val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
  val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
  val of_seq : (key * 'a) Seq.t -> 'a t
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val to_string : ('a -> string) -> 'a t -> string
  val hash : ('a -> int) -> 'a t -> int
  val of_list : (key * 'a) list -> 'a t
  val to_list : 'a t -> (key * 'a) list
  val union : 'a t -> 'a t -> 'a t
  val find_map : (key -> 'a -> bool) -> 'a t -> (key * 'a) option
  val fixpoint : ('a -> 'a -> bool) -> ('a t -> 'a t) -> 'a t -> 'a t
  val submap : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val add_bindings : (key * 'a) list -> 'a t -> 'a t
end
