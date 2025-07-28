open Generic

module Infrule : Infrule.S

val atomic_axioms : bool ref
(** Specifies whether axioms must be atomic (default: [false]) *)

val axioms : Proofrule.Make(Seq)(Infrule).t ref

val rules : Proofrule.Make(Seq)(Infrule).t ref
