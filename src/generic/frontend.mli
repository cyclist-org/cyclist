val exits : Cmdliner.Cmd.Exit.info list
(** Exit codes common to the prover commands, for [Cmdliner.Cmd.info ~exits]. *)

val debug_term : unit Cmdliner.Term.t
(** The [-d]/[-s]/[--id] output options, which every command has. {!Make.term}
    already includes them; commands that do not run the iterative-deepening
    prover combine this term directly. *)

module Make (Prover : Prover.S) : sig
  module Seq : Sequent.S with type t = Prover.Seq.t

  type result_t = TIMEOUT | NOT_FOUND | SUCCESS of Prover.Proof.t

  val show_proof : bool ref
  val use_dot : bool ref
  val timeout : int ref
  val minbound : int ref
  val maxbound : int ref

  val term :
    ?min_depth:int ->
    ?max_depth:int ->
    ?timeout_secs:int ->
    unit ->
    unit Cmdliner.Term.t
  (** The search and output options common to every prover command. Evaluating
      the term assigns the refs above. *)

  val common_term :
    ?min_depth:int ->
    ?max_depth:int ->
    ?timeout_secs:int ->
    unit ->
    unit Cmdliner.Term.t
  (** {!term} combined with {!Soundcheck.term}. *)

  val exit : result_t -> 'a
  val gather_stats : (unit -> 'a) -> 'a option
  val process_result : bool -> Seq.t -> Prover.Proof.t option option -> result_t

  val idfs :
    Prover.rule_t -> Prover.rule_t -> Prover.Seq.t -> Prover.Proof.t option

  val prove_seq : Prover.rule_t -> Prover.rule_t -> Seq.t -> result_t
end
