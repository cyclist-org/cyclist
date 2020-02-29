(** A list-based set container. *)

(** Create an ordered container whose underlying representation is a list. 
    The [to_list] operation takes constant time. *)
module Make (T : Utilsigs.BasicType) :
  Utilsigs.OrderedContainer with type elt = T.t
