(** A node in a cyclic proof. *)

module Make (Seq : Sigs.SEQUENT) : Sigs.NODE 
  with type seq_t = Seq.t 
