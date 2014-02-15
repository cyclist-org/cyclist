module Make (Seq: Sigs.SEQUENT) (Defs: Sigs.DEFINITIONS) : Sigs.PROVER
  with type sequent = Seq.t 
  with type ind_def_set=Defs.t
  with module Node = Proofnode.Make(Seq)
  with module Proof = Proof.Make(Proofnode.Make(Seq))
  with type proof = Proof.Make(Proofnode.Make(Seq)).t
