(** A cyclic abducer (cf. cyclic prover). *)

module Make (Seq: Sigs.SEQUENT) (Defs: Sigs.DEFS) : Sigs.ABDUCER
  with type defs_t = Defs.t
  with type proof_t = Proof.Make(Seq).t
  with type abdrule_t = Abdrule.Make(Seq)(Defs).t
  with module Seq = Seq
  with module Proof = Proof.Make(Seq)
