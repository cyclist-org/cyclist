(** Tactics for combining sequent-level rules. *)

module Make(Seq : Sigs.SEQUENT) : Sigs.SEQTACTICS 
  with type seq_t = Seq.t
