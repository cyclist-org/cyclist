module Make (Seq: Sigs.SEQUENT) : Sigs.PROVER2
  with module Seq = Seq
  with type rule_t = Proofrule.Make(Seq).t
  with module Proof = Proof.Make(Seq)
  with module Seqtactics = Seqtactics.Make(Seq)
