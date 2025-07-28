open Generic

val axioms : Proofrule.Make(Seq)(Lib.Strng).t ref

val rules : Proofrule.Make(Seq)(Lib.Strng).t ref

val use_cut : bool -> unit