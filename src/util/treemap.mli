(** An ordered map using the standard [Map] module. *)

module Make(T: Utilsigs.BasicType) : Utilsigs.OrderedMap with type key = T.t
(** Create an ordered map using a tree-based representation. *)