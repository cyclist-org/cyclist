module type S = sig

  type seq_t

  type infrule_t

  type proofrule_t

  type node_t

  module Proof : Proof.S with type seq_t := seq_t
                          and type infrule_t := infrule_t
                          and type node_t := node_t

  type result_t = TIMEOUT | NOT_FOUND | SUCCESS of Proof.t

  val show_proof : bool ref
  val use_dot : bool ref
  val timeout : int ref
  val minbound : int ref
  val maxbound : int option ref
  val speclist : (unit -> (string * Arg.spec * string) list) ref
  val usage : string ref
  val die : string -> (string * Arg.spec * string) list -> string -> 'a
  val exit : result_t -> 'a
  val gather_stats : (unit -> 'a) -> 'a option
  val idfs : proofrule_t -> proofrule_t -> seq_t -> Proof.t option
  val process_result : bool -> seq_t -> Proof.t option option -> result_t

  val prove_seq : proofrule_t -> proofrule_t -> seq_t -> result_t

end

module Make (Seq : Sequent.S) (Infrule : Infrule.S)
  : S with type seq_t := Seq.t
       and type infrule_t := Infrule.t
       and type proofrule_t := Proofrule.Make(Seq)(Infrule).t
       and type node_t := Proofnode.Make(Seq)(Infrule).t
       and module Proof := Proof.Make(Seq)(Infrule)
