(** A list-based multiset/bag. *)

(** Create an ordered bag whose underlying representation is a list. The
    [to_list] operation takes constant time. *)
module Make (T : Utilsigs.BasicType) :
  Utilsigs.OrderedContainer with type elt = T.t with type t = T.t list
