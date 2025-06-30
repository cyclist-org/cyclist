open Generic

val axioms : Prover.Make(Seq).rule_t ref

val rules : Prover.Make(Seq).rule_t ref

val use_cut : bool -> unit