(** A list-based set container. *)

module Make(T: Utilsigs.BasicType) : Utilsigs.OrderedContainer with type elt = T.t
(** Create an ordered container whose underlying representation is a list. 
    The [to_list] operation takes constant time. *)
