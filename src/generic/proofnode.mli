(** Provide a functor that defines a module around a proof node type. *)

module Make (Seq : Sigs.SEQUENT) : Sigs.NODE with type seq_t = Seq.t 
