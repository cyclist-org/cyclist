(** A node in a cyclic proof. *)

(** Proof node signature. *)
module type S = sig
  type t

  (** Sequent type used for building proof nodes. *)
  type seq_t

  (** Constructors. *)

  val mk_open : seq_t -> t
  (** [mk_open seq] creates an open Proof.t node labelled by [seq]. *)

  val mk_axiom : seq_t -> string -> t
  (** [mk_axiom seq descr] creates an axiom node labelled by
      sequent [seq] and description [descr].*)

  val mk_backlink : seq_t -> string -> int -> Tagpairs.t -> t
  (** [mk_backlink seq descr target vtts] creates a back-link node labelled by
      sequent [seq], description [descr], target index [target] and set of
      valid tag transitions (as pairs) [vtts].*)

  val mk_inf :
    seq_t -> string -> int list -> (Tagpairs.t * Tagpairs.t) list -> t
  (** [mk_inf seq descr subgoals back] creates an inference node labelled by
      sequent [seq], description [descr], a list of triples consisting of
      subgoal index, valid tag transitions and progressing tag transitions
      [subgoals].*)

  (** Destructors. *)

  val dest : t -> seq_t * string
  (** [dest n] returns (sequent, description). This works with all Proof.t nodes. *)

  val dest_backlink : t -> seq_t * string * int * Tagpairs.t
  (** [dest_backlink n] destroys a back-link node [n], otherwise raises [Invalid_arg].*)

  val dest_inf :
    t -> seq_t * string * int list * (Tagpairs.t * Tagpairs.t) list
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

  val to_abstract_node : t -> Soundcheck.abstract_node
  (** Convert Proof.t node to abstract node as in {!Soundcheck}. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty printing *)
end

module Make (Seq : Sequent.S) : S with type seq_t = Seq.t
