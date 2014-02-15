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
      
  val mk_open : Seq.t -> int -> t
  (** [mk_open seq parent] creates an open proof node labelled by [seq] with
      parent index set to [parent]. *) 

  val mk_axiom : Seq.t -> int -> string -> t
  (** [mk_axiom seq parent descr] creates an axiom proof node labelled by 
      sequent [seq], parent index [parent] and description [descr].*) 
  
  val mk_abd : Seq.t -> int -> int -> string -> t
  (** [mk_abd seq parent child descr] creates an abduction proof node labelled by 
      sequent [seq], parent index [parent], successor index [child] 
      and description [descr]. NB this will probably be removed. *) 
  
  val mk_backlink : Seq.t -> int -> int -> Util.TagPairs.t -> string -> t
  (** [mk_backlink seq parent target vtts descr] creates a back-link proof node labelled by 
      sequent [seq], parent index [parent], target index [target], set of 
      valid tag transitions (as pairs) [vtts] and description [descr].*) 
  
  val mk_inf :
    Seq.t -> int ->
    (int * Util.TagPairs.t * Util.TagPairs.t) list -> 
    string -> bool -> t
  (** [mk_inf seq parent subgoals descr] creates an inference proof node labelled by 
      sequent [seq], parent index [parent], a list of triples consisting of
      subgoal index, valid tag transitions and progressing tag transitions [subgoals] 
      and description [descr].*) 
  
  (** Destructors. *)
  
  val dest : t -> Seq.t * int * string
  (** [dest n] returns the triple (sequent, parent index, description).
      This works with all proof nodes. *)
   
  val dest_abd : t -> Seq.t * int * string * int
  (** [dest_abd n] destroys an abduction node [n], otherwise raises [Invalid_arg].*)
  
  val dest_backlink : t -> Seq.t * int * string * int * Util.TagPairs.t
  (** [dest_backlink n] destroys a back-link node [n], otherwise raises [Invalid_arg].*)

  val dest_inf : t -> 
    Seq.t * int * string * 
    (int * Util.TagPairs.t * Util.TagPairs.t) list * bool
  (** [dest_inf n] destroys an inference node [n], otherwise raises [Invalid_arg].*)

  (** Functions for checking the sort of a proof node. *)
  
  val is_open : t -> bool
  val is_axiom : t -> bool
  val is_abd : t -> bool
  val is_backlink : t -> bool
  val is_inf : t -> bool
  
  (** Auxiliary functions for getting information from all proof nodes. *)
  
  val get_seq : t -> Seq.t
  (** Get sequent labelling the node. *)
  val get_par : t -> int
  (** Get the parent node index of this node. *)
  val get_succs : t -> int list
  (** Get the successor node indices of this node. *)

  (** Convert proof node to abstract node as in {!Soundcheck} *)
  val to_abstract_node : t -> Soundcheck.abstract_node
      
  (** Pretty printing and Latex conversion. *)

  val pp :
    Format.formatter ->
    int -> t -> (Format.formatter -> int -> unit) -> unit
  val to_melt :
    bool -> int -> t -> (bool -> int -> Latex.t) -> Latex.t
end 
(** Proof node signature. *)


module type PROOF = 
sig
  type t 
  (** Proof type. *)
  
  module Node : NODE
  (** Proof node module underlying proof. *)
  
  val mk : Node.t -> t
  (** Constructor.  Takes an open or axiom node and creates a proof
      with that node at the root (0).  The parent field of the node
      must be 0. *)
  
  (** Other constructors. Many possible checks are performed
      and raise exceptions upon failing. *)
 
  val add_backlink : int -> Node.t -> t -> t
  val add_abd : int -> Node.t -> (int * Node.t) -> t -> t 
  val add_inf : int -> Node.t -> (int * Node.t) list -> t -> t

  (** Accessor functions. *)
  
  val find : int -> t -> Node.t
  val size : t -> int
  val mem : int -> t -> bool
  val fresh_idx : t -> int
  val fresh_idxs : 'a list -> t -> int list
  
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


module type PROVER =
sig
  type sequent
  type ind_def_set

  module Node : NODE
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

  type proof
  
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
