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
      
      [FIXME] Should back-links should have equal sequents to their targets?
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
  val mem : int -> t -> bool
  val fresh_idx : t -> int
  val fresh_idxs : 'a list -> t -> int list
  
  val get_ancestry : int -> t -> (int * node_t) list

  
  val check : t -> bool
  (** Check soundness. Proof does not need to be closed. *)
  
  val is_closed : t -> bool
  (** Are all nodes not open? *)
  
  val no_of_backlinks : t -> int
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
  type rule_t = seq_t -> ruleapp_t list
  
  val relabel : string -> rule_t -> rule_t
  val attempt : rule_t -> rule_t
  val compose : rule_t -> rule_t -> rule_t
  val first : rule_t list -> rule_t
  val repeat : rule_t -> rule_t
  val choice : rule_t list -> rule_t
end

module type PROOFRULE =
sig
  type seq_t
  type proof_t
  
  type axiom_f = seq_t -> string option
  type infrule_app = (seq_t * Util.TagPairs.t * Util.TagPairs.t) list * string
  type infrule_f = seq_t -> infrule_app list
  type t = int -> proof_t -> (int list * proof_t) Blist.t
  type backrule_f = seq_t -> seq_t -> (Util.TagPairs.t * string) list
  type select_f = int -> proof_t -> int list
      
  val mk_axiom : axiom_f -> t
  val mk_infrule : infrule_f -> t
  val mk_backrule : bool -> select_f -> backrule_f -> t
  
  val all_nodes : select_f
  val ancestor_nodes : select_f
  
  val fail : t
  val compose : t -> t -> t 
  val choice : t list -> t
  val attempt : t -> t
  val first : t list -> t 
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
  type seq_t
  type abdrule_t
  type proof_t
  type defs_t

  module Seq : SEQUENT
  module Proof : PROOF
  
  val bfs : 
    int -> int -> 
    abdrule_t -> seq_t -> defs_t ->
    (defs_t -> bool) -> 
    (proof_t * defs_t) option
  val print_proof_stats : Proof.t -> unit
  val melt_proof: out_channel -> Proof.t -> unit 
end
