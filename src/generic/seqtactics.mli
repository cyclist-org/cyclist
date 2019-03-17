(** Tactics for combining sequent-level rules. *)

module type S = sig
  type seq_t

  type ruleapp_t = (seq_t * Tagpairs.t * Tagpairs.t) list * string

  type t = seq_t -> ruleapp_t list

  val relabel : string -> t -> t

  val attempt : t -> t

  val compose : t -> t -> t

  val first : t list -> t

  val repeat : t -> t

  val choice : t list -> t
end

module Make (Seq : Sequent.S) : S with type seq_t = Seq.t
