(** A cyclic abducer (cf. cyclic prover). *)

module type S = sig

  type seq_t

  type infrule_t

  type proof_t

  type rule_t

  type defs_t

  module Abdrule : Abdrule.S with type seq_t := seq_t
                              and type infrule_t := infrule_t
                              and type proof_t := proof_t
                              and type rule_t := rule_t
                              and type defs_t := defs_t

  val bfs :
       int
    -> Abdrule.t
    -> seq_t
    -> defs_t
    -> (defs_t -> bool)
    -> (proof_t * defs_t) option

  val print_proof_stats : proof_t -> unit

end

module Make (Seq : Sequent.S) (Infrule : Infrule.S) (Defs : sig type t end) :
  S with type seq_t := Seq.t
     and type infrule_t := Infrule.t
     and type defs_t := Defs.t
     and type proof_t := Proof.Make(Seq)(Infrule).t
     and type rule_t := Proofrule.Make(Seq)(Infrule).t
     and module Abdrule := Abdrule.Make(Seq)(Infrule)(Defs)
