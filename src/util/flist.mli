(** A functorised list. *)

module Make(T: Utilsigs.BasicType) : Utilsigs.BasicType with type t = T.t list
(** Given a type with comparison, equality and printing facilities, build
    the same facilities for lists of that type. *)