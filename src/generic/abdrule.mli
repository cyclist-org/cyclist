(** An abduction rule used in an abductive cyclic prover. *)

module type S = sig
  type seq_t

  type proof_t

  type defs_t

  type rule_t

  type select_f = int -> proof_t -> int list

  type infrule_app = (seq_t * Tagpairs.t * Tagpairs.t) list * string

  type abdinfrule_f = seq_t -> defs_t -> defs_t list

  type abdbackrule_f = seq_t -> seq_t -> defs_t -> defs_t list

  type abdgenrule_f = seq_t -> defs_t -> (infrule_app * defs_t) list

  type t = int -> proof_t -> defs_t -> ((int list * proof_t) * defs_t) Blist.t

  val mk_abdinfrule : abdinfrule_f -> t

  val mk_abdbackrule : select_f -> abdbackrule_f -> t

  val mk_abdgenrule : abdgenrule_f -> t

  val fail : t

  val lift : rule_t -> t

  val compose : t -> t -> t

  val choice : t list -> t

  val attempt : t -> t

  val first : t list -> t
end

module Make (Seq : Sequent.S) (Defs : Defs.S) :
  S
  with type seq_t = Seq.t
  with type proof_t = Proof.Make(Seq).t
  with type rule_t = Proofrule.Make(Seq).t
  with type defs_t = Defs.t
