module Make (Seq: Sigs.SEQUENT) : Sigs.PROOF 
  with type seq_t = Seq.t
  with type node_t = Proofnode.Make(Seq).t 
   
