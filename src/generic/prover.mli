module Make (Seq: Sigs.SEQUENT) (Defs: Sigs.DEFS) : Sigs.PROVER
  with type sequent = Seq.t 
  with type ind_def_set=Defs.t
  with module Proof = Proof.Make(Seq)
