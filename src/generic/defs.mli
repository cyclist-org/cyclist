(** Inductive definitions signature. *)
module type S = sig
  type t

  val to_string : t -> string

  val pp : Format.formatter -> t -> unit
end
