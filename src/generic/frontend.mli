module Make :
  (Prover : Prover.S) ->
    sig
      module Seq :
        sig
          type t = Prover.Seq.t
          val equal : t -> t -> bool
          val equal_upto_tags : t -> t -> bool
          val tags : t -> Tags.t
          val to_string : t -> string
          val pp : Format.formatter -> t -> unit
        end
      type result_t = TIMEOUT | NOT_FOUND | SUCCESS of Prover.Proof.t
      val show_proof : bool ref
      val use_dot : bool ref
      val latex_path : string ref
      val open_file_for_append : bool ref
      val timeout : int ref
      val minbound : int ref
      val maxbound : int ref
      val speclist : (unit -> (string * Arg.spec * string) list) ref
      val usage : string ref
      val die : string -> (string * Arg.spec * string) list -> string -> 'a
      val exit : result_t -> 'a
      val gather_stats : (unit -> 'a) -> 'a option
      val process_result :
        bool -> Seq.t -> Prover.Proof.t option option -> result_t
      val idfs :
        Prover.rule_t ->
        Prover.rule_t -> Prover.Seq.t -> Prover.Proof.t option
      val prove_seq : Prover.rule_t -> Prover.rule_t -> Seq.t -> result_t
    end
