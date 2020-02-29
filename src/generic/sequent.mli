open Lib

(** Sequent signature used as input to most functors in Cyclist.*)
module type S = sig
  type t

  val equal : t -> t -> bool
  (** "Syntactic" equality.  Used to check that closing an open [NODE] is done
      via another one marked with a syntactically equal sequent. *)

  val equal_upto_tags : t -> t -> bool
  (** As [equal] but ignoring tags.  Used to check that the target of a
      backlink [NODE] is equal to that marking it, ignoring tags.*)

  val tags : t -> Tags.t
  (** Returns set of tags in sequent. *)

  val to_string : t -> string

  val pp : Format.formatter -> t -> unit
end
