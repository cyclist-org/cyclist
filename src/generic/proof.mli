(** A cyclic proof object. *)

open Lib

(** Proof signature. *)
module type S = sig
  (** Proof type. Invariants are:
      - Graph (all indices point to existing nodes).
      - Non-empty.
      - Tree rooted at 0.
      - Connected.
      - Leaves are open nodes, axioms or backlinks. *)
  type t

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

  val add_backlink : int -> string -> int -> Tagpairs.t -> t -> t

  val add_inf :
       int
    -> string
    -> (seq_t * Tagpairs.t * Tagpairs.t) list
    -> t
    -> int list * t

  val add_subprf : t -> int -> t -> t
  (** [add_subprf p idx p'] replaces the node [idx] in [p'] with a copy of [p].
      N.B. The following should hold, otherwise exceptions will be raised:
        - [idx] should be an open node;
        - the sequent of [idx] should be identical to the root sequent of [p]. *)

  val extract_subproof : int -> t -> t
  (** [extract_subproof idx prf] returns [prf] rearranged such that [idx] is
      the root node. *)

  (** Accessor functions. *)

  val find : int -> t -> node_t

  val get_seq : int -> t -> seq_t

  val size : t -> int

  val num_backlinks : t -> int

  (* val mem : int -> t -> bool *)

  val fresh_idx : t -> int

  val fresh_idxs : 'a list -> t -> int list

  val get_ancestry : int -> t -> (int * node_t) list

  val is_closed_at : t -> int -> bool

  val check : t -> bool
  (** Check soundness. Proof does not need to be closed. *)

  val is_closed : t -> bool
  (** Are all nodes not open? *)

  val to_list : t -> (int * node_t) list

  (** Output functions. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-print the proof. *)

  val to_string : t -> string
  (** Return a string representation of the proof *)

  val pp_dot : Format.formatter -> t -> unit
  (** Format a representation of the proof in the DOT graph format. *)

  val to_dot_string : t -> string
  (** Return a string containing a DOT representation of the proof. *)
end

module Make (Seq : Sequent.S) :
  S with type seq_t = Seq.t with type node_t = Proofnode.Make(Seq).t
