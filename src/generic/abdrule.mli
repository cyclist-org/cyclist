(** An abduction rule used in an abductive cyclic prover. *)

module Make (Seq : Sigs.SEQUENT) (Defs : Sigs.DEFS) :
  Sigs.ABDRULE
  with type seq_t = Seq.t
  with type proof_t = Proof.Make(Seq).t
  with type rule_t = Proofrule.Make(Seq).t
  with type defs_t = Defs.t
