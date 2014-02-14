module Make (Seq: Sigs.S) (Defs: Sigs.D) : Sigs.P
  with type sequent=Seq.t with type ind_def_set=Defs.t
