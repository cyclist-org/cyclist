(** Operations on pairs. *)

val mk : 'a -> 'b -> 'a * 'b
(** Pair constructor. *)

val left : 'a * 'b -> 'a
(** Pair destructor. *)

val right : 'a * 'b -> 'b
(** Pair destructor. *)

val map : ('a -> 'b) -> 'a * 'a -> 'b * 'b
(** Apply a function to both members individually and put results in a new pair. *)

val map_left : ('a -> 'c) -> 'a * 'b -> 'c * 'b
(** Apply a function to the left-hand component only *)

val map_right : ('b -> 'c) -> 'a * 'b -> 'a * 'c
(** Apply a function to the right-hand component only *)

val apply : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
(** Apply a function taking two arguments to the members of a pair. *)

val conj : bool * bool -> bool
(** Compute the conjunction of a pair of booleans. *)

val disj : bool * bool -> bool
(** Compute the disjunction of a pair of booleans. *)

val swap : 'a * 'b -> 'b * 'a
(** Swap around the members of a pair. *)

val fold : ('a -> 'b -> 'b) -> 'a * 'a -> 'b -> 'b
(** Fold a function over the members of a pair. *)

val both : bool * bool -> bool
(** Alias for [conj] *)

val either : bool * bool -> bool
(** Alias for [disj] *)

(** Create functions for equality, comparison, hashing and printing for a
    pair of types. *)
module Make (T : Utilsigs.BasicType) (S : Utilsigs.BasicType) :
  Utilsigs.BasicType with type t = T.t * S.t
