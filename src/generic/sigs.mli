(** Core module signatures used in Cyclist. *)

module type SEQUENT = 
sig
  type t
  (** The sequent type. *)
  
  val equal : t -> t -> bool
  (** Equality for sequents. *)
  
  val tags : t -> Util.Tags.t
  (** Returns set of tags in sequent. *)

  (** Pretty printing, conversions to Latex and string. *)
  
  val to_string : t -> string
  val to_melt: t -> Latex.t
  val pp : Format.formatter -> t -> unit
end
(** Sequent signature used as input to most functors in Cyclist.*)

module type DEFINITIONS =
sig
  type t
  val to_string : t -> string
  val to_melt: t -> Latex.t
  val pp : Format.formatter -> t -> unit
end
(** Inductive definitions signature. *)

module type NODE = 
sig
  type t
  (** Proof node type. *)

  module Seq : SEQUENT
  (** Sequent module used for building proof nodes. *)
      
  (** Constructors. *)
      
  val mk_open : Seq.t -> t
  (** [mk_open seq] creates an open Proof.t node labelled by [seq]. *) 

  val mk_axiom : Seq.t -> string -> t
  (** [mk_axiom seq descr] creates an axiom node labelled by 
      sequent [seq] and description [descr].*) 
  
  val mk_abd : Seq.t -> string -> int -> t
  (** [mk_abd seq descr child] creates an abduction node labelled by 
      sequent [seq], description [descr] and successor index [child]. 
      NB this will probably be removed. *) 
  
  val mk_backlink : Seq.t -> string -> int -> Util.TagPairs.t -> t
  (** [mk_backlink seq descr target vtts] creates a back-link node labelled by 
      sequent [seq], description [descr], target index [target] and set of 
      valid tag transitions (as pairs) [vtts].*) 
  
  val mk_inf :
    Seq.t -> string -> (int * Util.TagPairs.t * Util.TagPairs.t) list -> t
  (** [mk_inf seq descr subgoals back] creates an inference node labelled by 
      sequent [seq], description [descr], a list of triples consisting of
      subgoal index, valid tag transitions and progressing tag transitions 
      [subgoals].*) 
  
  (** Destructors. *)
  
  val dest : t -> Seq.t * string
  (** [dest n] returns (sequent, description). This works with all Proof.t nodes. *)
   
  val dest_abd : t -> Seq.t * string * int
  (** [dest_abd n] destroys an abduction node [n], otherwise raises [Invalid_arg].*)
  
  val dest_backlink : t -> Seq.t * string * int * Util.TagPairs.t
  (** [dest_backlink n] destroys a back-link node [n], otherwise raises [Invalid_arg].*)

  val dest_inf : t -> 
    Seq.t * string * (int * Util.TagPairs.t * Util.TagPairs.t) list
  (** [dest_inf n] destroys an inference node [n], otherwise raises [Invalid_arg].*)

  (** Functions for checking the sort of a node. *)
  
  val is_open : t -> bool
  val is_axiom : t -> bool
  val is_abd : t -> bool
  val is_backlink : t -> bool
  val is_inf : t -> bool
  
  (** Auxiliary functions for getting information from all nodes. *)
  
  val get_seq : t -> Seq.t
  (** Get sequent labelling the node. *)
  val get_succs : t -> int list
  (** Get the successor node indices of this node. *)

  (** Convert Proof.t node to abstract node as in {!Soundcheck}. *)
  val to_abstract_node : t -> Soundcheck.abstract_node
      
  (** Pretty printing and Latex conversion. *)

  val pp :
    Format.formatter ->
    int -> t -> (Format.formatter -> int -> unit) -> unit
  val to_melt :
    bool -> int -> t -> (bool -> int -> Latex.t) -> Latex.t
  (** Convert to Latex.  The first parameter is true when the node is the root. *)
end 
(** Proof node signature. *)


module type PROOF = 
sig
  type t 
  (** Proof type. Invariants are:
      - Graph (all indices point to existing nodes).
      - Non-empty. 
      - Rooted at 0. 
      - Connected. *)
  
  module Node : NODE
  (** Proof node module underlying proof. *)
  
  val mk : Node.Seq.t -> t
  (** Constructor.  Takes a sequent and makes an open node at the root (0).*)
  
  (** Other constructors, which return the proof and the indices of new
      subgoals, if any, which are new, open nodes.
      
      All of these take an index to an open node, and a description plus
      more arguments appropriate to the type of constructor.  The open
      node will be replaced by another node with the same index, and its
      description set to the parameter value.      
      
      [FIXME] Should back-links should have equal sequents to their targets?
      *)

  val add_axiom : int -> string -> t -> t
  val add_backlink : int -> string -> int -> Util.TagPairs.t -> t -> t
  val add_abd : int -> string -> t -> (t * int) 
  val add_inf : 
    int -> string -> 
    (Node.Seq.t * Util.TagPairs.t * Util.TagPairs.t) list -> t -> 
    (t * int list)

  (** Accessor functions. *)
  
  val find : int -> t -> Node.t
  val get_seq : int -> t -> Node.Seq.t
  val size : t -> int
  val mem : int -> t -> bool
  val fresh_idx : t -> int
  val fresh_idxs : 'a list -> t -> int list
  
  val get_ancestry : int -> t -> (int * Node.t) list

  
  val check : t -> bool
  (** Check soundness. Proof does not need to be closed. *)
  
  val is_closed : t -> bool
  (** Are all nodes not open? *)
  
  val no_of_backlinks : t -> int
  val to_list : t -> (int * Node.t) list

  (** Output functions. *)
  
  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
  val to_melt : t -> Latex.t
end
(** Proof signature. *)

module type SEQTACTICS =
sig
  module Seq : SEQUENT

  type ruleapp_t = (Seq.t * Util.TagPairs.t * Util.TagPairs.t) list * string
  type rule_t = Seq.t -> ruleapp_t list
  
  val attempt : rule_t -> rule_t
  val compose : rule_t -> rule_t -> rule_t
  val first : rule_t list -> rule_t
  val repeat : rule_t -> rule_t
end

module type RULES =
sig
  module Proof : PROOF
  
  type axiom_f = Proof.Node.Seq.t -> string option
  type infrule_app = (Proof.Node.Seq.t * Util.TagPairs.t * Util.TagPairs.t) list * string
  type infrule_f = Proof.Node.Seq.t -> infrule_app list
  type infrule = int -> Proof.t -> (Proof.t * int list) list
  type backrule_f = 
    Proof.Node.Seq.t -> Proof.Node.Seq.t -> (Util.TagPairs.t * string) list
  type select_f = int -> Proof.t -> int list
      
  val mk_axiom : axiom_f -> infrule
  val mk_infrule : infrule_f -> infrule
  val mk_backrule : select_f -> backrule_f -> infrule
  
  val all_nodes : select_f
  val ancestor_nodes : select_f
end

module type PROOFTACTICS =
sig
  module Proof : PROOF
  
  type infrule = int -> Proof.t -> (Proof.t * int list) list
  
  val compose : infrule -> infrule -> infrule 
end


module type PROVER =
sig
  type sequent
  type ind_def_set

  module Proof : PROOF
  
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
  
  val idfs : sequent -> Proof.t option

  val abduce :
    sequent ->
    ind_def_set ->
    (ind_def_set -> proof_rule list) ->
    (ind_def_set -> bool) ->
      (Proof.t * ind_def_set) option

  val print_proof_stats : Proof.t -> unit
  val melt_proof: out_channel -> Proof.t -> unit 

  module Seq_tacs :
    sig
      val try_tac : rule_fun -> rule_fun
      val then_tac : rule_fun -> rule_fun -> rule_fun
      val repeat_tac : rule_fun -> rule_fun
      val first : rule_fun list -> rule_fun
      val seq : rule_fun list -> rule_fun
      val or_tac : rule_fun list -> rule_fun
			val opt : rule_fun -> rule_fun
    end

  module Proof_tacs :
    sig
      val try_tac : proof_rule -> proof_rule
      val then_tac : proof_rule -> proof_rule -> proof_rule
      val first : proof_rule list -> proof_rule
      val seq : proof_rule list -> proof_rule
      val or_tac : proof_rule list -> proof_rule
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
