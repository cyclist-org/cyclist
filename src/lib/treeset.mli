(** An ordered container based on the standard [Set] module. *)

(** Create an ordered container. *)
module Make (T : Utilsigs.BasicType) :
  Utilsigs.OrderedContainer with type elt = T.t
