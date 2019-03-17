(** A cyclic abducer (cf. cyclic prover). *)

module type S = sig
  type abdrule_t

  type proof_t

  type defs_t

  module Seq : Sequent.S

  module Proof : Proof.S

  val bfs :
       int
    -> abdrule_t
    -> Seq.t
    -> defs_t
    -> (defs_t -> bool)
    -> (proof_t * defs_t) option

  val print_proof_stats : Proof.t -> unit
end

module Make (Seq : Sequent.S) (Defs : Defs.S) :
  S
  with type defs_t = Defs.t
  with type proof_t = Proof.Make(Seq).t
  with type abdrule_t = Abdrule.Make(Seq)(Defs).t
  with module Seq = Seq
  with module Proof = Proof.Make(Seq)
