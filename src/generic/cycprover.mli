module type S =
sig
  type t
  val equal : t -> t -> bool
  val tags : t -> Util.Tags.t
  val to_string : t -> string
  val to_melt: t -> Latex.t
  val pp : Format.formatter -> t -> unit
end

module type D =
sig
  type t
  val to_string : t -> string
  val to_melt: t -> Latex.t
  val pp : Format.formatter -> t -> unit
end

module type P =
sig
  type sequent
  type ind_def_set

  type axiom_fun = sequent -> bool
  type rule_app = (sequent * Util.TagPairs.t * Util.TagPairs.t) list
  type rule_fun = sequent -> rule_app list
  type match_fun = sequent -> sequent -> Util.TagPairs.t option
  type abd_inf_fun = sequent -> ind_def_set -> ind_def_set list
  type abd_match_fun = sequent -> sequent -> ind_def_set -> ind_def_set list
  type gen_fun = sequent -> ind_def_set -> (rule_app * ind_def_set) list
  type axiom
  type proof_rule

  val mk_axiom : axiom_fun -> string -> axiom
  val mk_inf_rule : rule_fun -> string -> proof_rule
  val mk_back_rule : match_fun -> string -> proof_rule
  val mk_abd_inf_rule : abd_inf_fun -> string -> proof_rule
  val mk_abd_back_rule : abd_match_fun -> string -> proof_rule
  val mk_gen_rule : gen_fun -> string -> proof_rule

  val descr_rule : proof_rule -> string

  (* type proof *)
  (* type proof_subnode =                                                       *)
  (*   | OpenNode                                                               *)
  (*   | AxiomNode of axiom                                                     *)
  (*   | InfNode of                                                             *)
  (*     int list * Util.TagPairs.t list * Util.TagPairs.t list * string * bool *)
  (*   | BackNode of int * Util.TagPairs.t * string                             *)
  (*   | AbdNode of int * string                                                *)
  (* type proof_node =                                                          *)
  (*   {                                                                        *)
  (*     seq: sequent;                                                          *)
  (*     parent: int;                                                           *)
  (*     node: proof_subnode;                                                   *)
  (*   }                                                                        *)
  type proof_node
  type proof = proof_node Util.Int.Map.t
  
  val get_seq : proof_node -> sequent

  val idfs : sequent -> proof option

  val abduce :
    sequent ->
    ind_def_set ->
    (ind_def_set -> proof_rule list) ->
    (ind_def_set -> bool) ->
      (proof * ind_def_set) option

  val pp_proof : Format.formatter -> proof -> unit
  val print_proof : proof -> unit
  val print_proof_stats : proof -> unit
  val to_melt : proof -> Latex.t
  val melt_proof: out_channel -> proof -> unit 

  module Seq_tacs :
    sig
      val try_tac : rule_fun -> rule_fun
      val then_tac : rule_fun -> rule_fun -> rule_fun
      val repeat_tac : rule_fun -> rule_fun
      val first : rule_fun list -> rule_fun
      val seq : rule_fun list -> rule_fun
      val angelic_or_tac : rule_fun list -> rule_fun
			val opt : rule_fun -> rule_fun
    end

  module Proof_tacs :
    sig
      val try_tac : proof_rule -> proof_rule
      val then_tac : proof_rule -> proof_rule -> proof_rule
      val first : proof_rule list -> proof_rule
      val seq : proof_rule list -> proof_rule
      val angelic_or_tac : proof_rule list -> proof_rule
      val repeat_tac : proof_rule -> proof_rule
			val opt : proof_rule -> proof_rule
    end

  val axiomset : axiom list ref
  val ruleset : proof_rule list ref
  val ancestral_links_only : bool ref
  val minbound : int ref
  val maxbound : int ref
  val lazy_soundness_check : bool ref
  val backtrackable_backlinks : bool ref
  val expand_proof : bool ref
end
