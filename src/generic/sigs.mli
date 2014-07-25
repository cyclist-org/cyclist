(** Core module signatures used in Cyclist. *)

module type SEQUENT = 
sig
  type t
  val equal : t -> t -> bool
  
  val tags : t -> Util.Tags.t
  (** Returns set of tags in sequent. *)

  val to_string : t -> string
  val to_melt: t -> Latex.t
  val pp : Format.formatter -> t -> unit
end
(** Sequent signature used as input to most functors in Cyclist.*)

module type DEFS =
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

  type seq_t
  (** Sequent type used for building proof nodes. *)
      
  (** Constructors. *)
      
  val mk_open : seq_t -> t
  (** [mk_open seq] creates an open Proof.t node labelled by [seq]. *) 

  val mk_axiom : seq_t -> string -> t
  (** [mk_axiom seq descr] creates an axiom node labelled by 
      sequent [seq] and description [descr].*) 
  
  val mk_backlink : seq_t -> string -> int -> Util.TagPairs.t -> t
  (** [mk_backlink seq descr target vtts] creates a back-link node labelled by 
      sequent [seq], description [descr], target index [target] and set of 
      valid tag transitions (as pairs) [vtts].*) 
  
  val mk_inf :
    seq_t -> string -> (int * Util.TagPairs.t * Util.TagPairs.t) list -> t
  (** [mk_inf seq descr subgoals back] creates an inference node labelled by 
      sequent [seq], description [descr], a list of triples consisting of
      subgoal index, valid tag transitions and progressing tag transitions 
      [subgoals].*) 
  
  (** Destructors. *)
  
  val dest : t -> seq_t * string
  (** [dest n] returns (sequent, description). This works with all Proof.t nodes. *)
   
  val dest_backlink : t -> seq_t * string * int * Util.TagPairs.t
  (** [dest_backlink n] destroys a back-link node [n], otherwise raises [Invalid_arg].*)

  val dest_inf : t -> 
    seq_t * string * (int * Util.TagPairs.t * Util.TagPairs.t) list
  (** [dest_inf n] destroys an inference node [n], otherwise raises [Invalid_arg].*)

  (** Functions for checking the sort of a node. *)
  
  val is_open : t -> bool
  val is_axiom : t -> bool
  val is_backlink : t -> bool
  val is_inf : t -> bool
  
  (** Auxiliary functions for getting information from all nodes. *)
  
  val get_seq : t -> seq_t
  (** Get sequent labelling the node. *)
  val get_succs : t -> int list
  (** Get the successor node indices of this node. *)

  (** Convert Proof.t node to abstract node as in {!Soundcheck}. *)
  val to_abstract_node : t -> Soundcheck.abstract_node
      
  (** Pretty printing and Latex conversion. *)

  val pp : Format.formatter -> t -> unit
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
      - Connected.
      - Leaves are open nodes, axioms or backlinks. *)
  
  type seq_t
  type node_t
  
  val mk : seq_t -> t
  (** Constructor.  Takes a sequent and makes an open node at the root (0).*)
  
  (** Other constructors, which return the indices of new
      subgoals, if any, which are new, open nodes, and the new proof.
      
      All of these take an index to an open node, and a description plus
      more arguments appropriate to the type of constructor.  The open
      node will be replaced by another node with the same index, and its
      description set to the parameter value.      
      
      Back-links *must* have equal sequents to their targets, otherwise
      an exception will be thrown.`
      *)

  val add_axiom : int -> string -> t -> t
  val add_backlink : int -> string -> int -> Util.TagPairs.t -> t -> t
  val add_inf : 
    int -> string -> 
    (seq_t * Util.TagPairs.t * Util.TagPairs.t) list -> t -> 
    (int list * t)

  (** Accessor functions. *)
  
  val find : int -> t -> node_t
  val get_seq : int -> t -> seq_t
  val size : t -> int
  (* val mem : int -> t -> bool *)
  val fresh_idx : t -> int
  val fresh_idxs : 'a list -> t -> int list
  
  val get_ancestry : int -> t -> (int * node_t) list

  
  val check : t -> bool
  (** Check soundness. Proof does not need to be closed. *)
  
  val is_closed : t -> bool
  (** Are all nodes not open? *)
  
  val to_list : t -> (int * node_t) list

  (** Output functions. *)
  
  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
  val to_melt : t -> Latex.t
end
(** Proof signature. *)

module type SEQTACTICS =
sig
  type seq_t

  type ruleapp_t = (seq_t * Util.TagPairs.t * Util.TagPairs.t) list * string
  type t = seq_t -> ruleapp_t list
  
  val relabel : string -> t -> t
  val attempt : t -> t
  val compose : t -> t -> t
  val first : t list -> t
  val repeat : t -> t
  val choice : t list -> t
end

module type PROOFRULE =
sig
  type seq_t
  type proof_t
  
  type axiom_f = seq_t -> string option
  type infrule_app = (seq_t * Util.TagPairs.t * Util.TagPairs.t) list * string
  type infrule_f = seq_t -> infrule_app list
  type backrule_f = seq_t -> seq_t -> (Util.TagPairs.t * string) list
  type select_f = int -> proof_t -> int list

  (** The type of proof rules:
        Proof rules are functions that take an open node in a proof (i.e. an int
				identifying the node, and the proof structure containing the node) and
				return a list of new proofs that are obtained by applying the rule in all
				applicable ways to the open node in the provided proof, along with a list
				for each new proof of the newly added open nodes (i.e. the premises) 
  **)
  type t = int -> proof_t -> (int list * proof_t) Blist.t

		  
  (** Axioms are modeled as functions that return [Some string] when the 
      input sequent is an instance of an axiom described by [string], else
      [None]. *) 
  val mk_axiom : axiom_f -> t
	
  (** Rules are functions that break down a sequent to a choice of applications
      where each application is a list of premises, including tag information,
      and a description. *)
  val mk_infrule : infrule_f -> t
	
  (** Backlink rules take:
      - a boolean [eager] 
      - a selection function [s]
      - a matching function [m]
      The selection function is applied on the current subgoal and proof, and
      a list of goal indices is returned that represents possible back-link
      targets. This allows flexibility e.g., in changing from ancestral-only
      back-links to general ones.  
      
      Next, [m] is applied to every pair consisting of the current subgoal
      and a goal returned from the selection function. [m] returns a list of
      tag information and string descriptions, each describing a different
      way to form a back-link.
      
      If [eager] is true then the first result in this iteration will be chosen
      and no back-tracking will even happen over later possible matches. 
      Otherwise all possible matches are returned as different choices.
      *) 
  val mk_backrule : bool -> select_f -> backrule_f -> t
  
  (** Ready-made selection functions doing the obvious. *)
  val all_nodes : select_f
  val ancestor_nodes : select_f
  
  (** The rule that always fails. *)
  val fail : t
  (** Return current subgoal and proof as application. *)
  val identity : t
  
  (** Try a rule and if it fails act as [identity]. *)
  val attempt : t -> t
  (** Apply the second rule on all premises generated by applying the first. *)
  val compose : t -> t -> t 
  
  (** Apply a list of rules on current subgoal and return all applications. *)
  val choice : t list -> t
  (** Try rules from a list until the first rule is found that has some 
      applications on current sugboal and return only those. *)
  val first : t list -> t 
  (** Apply a sequence of rules iteratively through [compose]. *)
  val sequence : t list -> t 
end

module type ABDRULE =
sig
  type seq_t
  type proof_t
  type defs_t
  type rule_t
  
  type select_f = int -> proof_t -> int list
  type infrule_app = (seq_t * Util.TagPairs.t * Util.TagPairs.t) list * string
  
  type abdinfrule_f = seq_t -> defs_t -> defs_t list
  type abdbackrule_f = seq_t -> seq_t -> defs_t -> defs_t list
  type abdgenrule_f = seq_t -> defs_t -> (infrule_app * defs_t) list
  
  type t = int -> proof_t -> defs_t -> ((int list * proof_t) * defs_t) Blist.t

  val mk_abdinfrule : abdinfrule_f -> t
  val mk_abdbackrule : select_f -> abdbackrule_f -> t
  val mk_abdgenrule : abdgenrule_f -> t

  val fail : t
  val lift : rule_t -> t
  val compose : t -> t -> t
  val choice : t list -> t
  val attempt : t -> t
  val first : t list -> t
end

module type PROVER =
sig
  type rule_t
  
  module Seq : SEQUENT
  module Proof : PROOF

  val idfs : int -> int -> rule_t -> rule_t -> Seq.t -> Proof.t option  
  val print_proof_stats : Proof.t -> unit
  val melt_proof: out_channel -> Proof.t -> unit 
   
end

module type ABDUCER =
sig
  type abdrule_t
  type proof_t
  type defs_t

  module Seq : SEQUENT
  module Proof : PROOF
  
  val bfs : 
    int -> int -> 
    abdrule_t -> Seq.t -> defs_t ->
    (defs_t -> bool) -> 
    (proof_t * defs_t) option
  val print_proof_stats : Proof.t -> unit
  val melt_proof: out_channel -> Proof.t -> unit 
end
