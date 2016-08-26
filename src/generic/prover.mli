(** A cyclic prover object. *)

module Make (Seq: Sigs.SEQUENT) : Sigs.PROVER
  with module Seq = Seq
  with type rule_t = Proofrule.Make(Seq).t
  with module Proof = Proof.Make(Seq)
