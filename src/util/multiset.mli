(** A multiset container. *)

module Make(T: Utilsigs.BasicType) : 
  Utilsigs.OrderedContainer with type elt = T.t 
(** Create an ordered container that behaves like a bag ([add] always increases
    size by one). The underlying representation is list-based, so operations
    like [to_list] take constant time. *)