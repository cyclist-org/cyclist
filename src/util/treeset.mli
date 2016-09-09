(** An ordered container based on the standard [Set] module. *)

module Make(T: Utilsigs.BasicType) : Utilsigs.OrderedContainer with type elt = T.t
(** Create an ordered container. *)
