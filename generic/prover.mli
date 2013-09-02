module Make (SEQ: Cycprover.S) (DEFS: Cycprover.D) : Cycprover.P
  with type sequent=SEQ.t with type ind_def_set=DEFS.t
