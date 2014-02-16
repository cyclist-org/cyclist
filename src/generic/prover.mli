module Make (Seq: Sigs.SEQUENT) (Defs: Sigs.DEFINITIONS) : Sigs.PROVER
  with type sequent = Seq.t 
  with type ind_def_set=Defs.t
  with module Proof = Proof.Make(Proofnode.Make(Seq))
