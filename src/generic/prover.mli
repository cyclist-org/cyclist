(** A cyclic prover object. *)

module type S = sig
  type rule_t

  module Seq : Sequent.S

  module Proof : Proof.S

  val last_search_depth : int ref

  val idfs : int -> int -> rule_t -> rule_t -> Seq.t -> Proof.t option

  (* val bfs : int -> rule_t -> rule_t -> Seq.t -> Proof.t option   *)
  val print_proof_stats : Proof.t -> unit
end

module Make (Seq : Sequent.S) :
  S
  with module Seq = Seq
  with type rule_t = Proofrule.Make(Seq).t
  with module Proof = Proof.Make(Seq)
