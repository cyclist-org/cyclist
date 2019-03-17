(** A proofrule used by a cyclic prover. *)

module Make (Seq : Sigs.SEQUENT) :
  Sigs.PROOFRULE with type seq_t = Seq.t with type proof_t = Proof.Make(Seq).t
