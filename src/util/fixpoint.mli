(** Provide a fixpoint function given an equality predicate. *)

module Make(T: sig type t val equal : t -> t -> bool end) :
sig
  val fixpoint : (T.t ->T.t) -> T.t -> T.t 
end
(** Build the fixpoint function of a type that comes with an equality predicate. *)