(** A cyclic prover object. *)

module type S = sig

  type seq_t
  type rule_t
  type proof_t

  (* module Seq : Sequent.S

  module Proof : Proof.S with type seq_t := Seq.t
                          and type infdata_t := infdata_t
                          and type *)

  val last_search_depth : int ref

  val idfs : int -> int -> rule_t -> rule_t -> seq_t -> proof_t option

  (* val bfs : int -> rule_t -> rule_t -> Seq.t -> Proof.t option   *)
  val print_proof_stats : proof_t -> unit
end

module Make (Seq : Sequent.S) (Infrule : Infrule.S)
    : S with type seq_t := Seq.t
         and type rule_t := Proofrule.Make(Seq)(Infrule).t
         and type proof_t := Proof.Make(Seq)(Infrule).t
