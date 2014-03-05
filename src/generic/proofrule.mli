module Make(Seq : Sigs.SEQUENT) : Sigs.PROOFRULES 
  with type seq_t = Seq.t
  with type proof_t = Proof.Make(Seq).t
