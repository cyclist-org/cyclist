module type S = sig

  type seq_t

  type infrule_t

  type proofrule_t

  type node_t

  module Proof : Proof.S with type seq_t := seq_t
                          and type infrule_t := infrule_t
                          and type node_t := node_t

  module Prover : Prover.S with type seq_t := seq_t
                          and type rule_t := proofrule_t
                          and type proof_t := Proof.t

  type result_t =
    seq_t * [ `TIMEOUT | `NOT_FOUND | `SUCCESS of Proof.t ] * Stats.t * int

  val show_proof : bool ref
  val use_dot : bool ref
  val timeout : int ref
  val minbound : int ref
  val maxbound : int option ref
  val speclist : (unit -> (string * Arg.spec * string) list) ref
  val usage : string ref
  val die : string -> (string * Arg.spec * string) list -> string -> 'a
  val exit : result_t -> unit

  val prove_seq : proofrule_t -> proofrule_t -> seq_t -> Proof.t option
  (** [prove_seq axioms rules seq] runs the prover on the given sequent [seq]
      with the given [axioms] and inference [rules]. *)

  val process_result : seq_t -> Proof.t option Stats.result -> result_t
  (** Converts a [Stat.result] value wrapping an optional proof object into a
      [result_t] value. *)

  val pp_result : Format.formatter -> result_t -> unit
  (** Pretty-print result *)

  val print_result : result_t -> unit
  (** Print result to standard output *)

end

module Make (Seq : Sequent.S) (Infrule : Infrule.S)
  : S with type seq_t := Seq.t
       and type infrule_t := Infrule.t
       and type proofrule_t := Proofrule.Make(Seq)(Infrule).t
       and type node_t := Proofnode.Make(Seq)(Infrule).t
       and module Proof := Proof.Make(Seq)(Infrule)
