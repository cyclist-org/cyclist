open Generic

include Sequent.S

val pp_no_tags : Format.formatter -> t -> unit

val of_string : string -> t

val antecedent : t -> Form.t list
val consequent : t -> Form.t list

(* The position of the first consequent formula *)
val right_start : t -> int

val get_tag : int -> t -> Tags.Elt.t

val zip_tags : t -> t -> Tagpairs.t

val of_lists : Form.t list * Form.t list -> t
val with_consequent : Form.t list -> t -> t

val nth : t -> int -> Form.t
val nth_opt : t -> int -> Form.t option

val exists_left : (Form.t -> bool) -> t -> bool
val exists_right : (Form.t -> bool) -> t -> bool

val forall_left : (Form.t -> bool) -> t -> bool
val forall_right : (Form.t -> bool) -> t -> bool

val find_index_left : (Form.t -> bool) -> t -> int option
val find_index_right : (Form.t -> bool) -> t -> int option

val find_indices_left : (Form.t -> bool) -> t -> int list
val find_indices_right : (Form.t -> bool) -> t -> int list

val insert : Form.t -> int -> t -> t
val insert_many : Form.t list -> int -> t -> t

val remove_at : int -> t -> t
(** [remove_at idx seq] returns [seq] with the formula at position [idx] removed. *)

val remove_many_at : int -> int -> t -> t
(** [remove_many_at idx len seq] returns [seq] with [len] formulas removed,
    starting at position [idx]. *)

(** [filter p seq] removes all formulas (from both antecedent and consequent)
    from [seq] that do not satisfy the predicate [p] (i.e. all formulas [f] for
    which [p f] returns [false]), and returns the result. *)
val filter : (Form.t -> bool) -> t -> t

(** [filter_left p seq] removes all formulas from the antecedent of [seq] that
    do not satisfy the predicate [p] (i.e. all formulas [f] for which [p f]
    returns [false]), and returns the result. *)
val filter_left : (Form.t -> bool) -> t -> t

(** [filter_right p seq] removes all formulas from the consequent of [seq] that
    do not satisfy the predicate [p] (i.e. all formulas [f] for which [p f]
    returns [false]), and returns the result. *)
val filter_right : (Form.t -> bool) -> t -> t

(** [remove p seq] removes all formulas (from both antecedent and consequent)
    from [seq] that satisfy the predicate [p] (i.e. all formulas [f] for which
    [p f] returns [true]), and returns the result.
    It is equivalent to [filter (fun f -> not p) seq]. *)
val remove : (Form.t -> bool) -> t -> t

(** [remove_left p seq] removes all formulas from the antecedent of [seq] that
    satisfy the predicate [p] (i.e. all formulas [f] for which [p f] returns
    [true]), and returns the result.
    It is equivalent to [filter_left (fun f -> not p) seq]. *)
val remove_left : (Form.t -> bool) -> t -> t

(** [remove_right p seq] removes all formulas from the consequent of [seq] that
    satisfy the predicate [p] (i.e. all formulas [f] for which [p f] returns
    [true]), and returns the result.
    It is equivalent to [filter_right (fun f -> not p) seq]. *)
val remove_right : (Form.t -> bool) -> t -> t

val take_left : int -> t -> t
val take_right : int -> t -> t
val drop_left : int -> t -> t
val drop_right : int -> t -> t

val all_left_splits : t -> (t * t) list

(* Predicates *)

val is_axiomatic : ?nonatomic:bool -> t -> bool

val is_empty_left : t -> bool
val is_empty_right : t -> bool