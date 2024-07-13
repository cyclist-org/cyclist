(** Provides an abstract view of a proof as a graph and allows checking its
    soundness. *)

open Lib

(** Abstract proof node type.
    The only information stored is
      - a set of tags (integers)
      - whether the node is a bud
      - a list of tuples of:
          + successor,
          + set of valid tag transitions and
          + set of progressing tag transitions. *)
type abstract_node

(** The type of abstract proofs: a map from ints to nodes.
    NB the root is always at 0. *)
type t = abstract_node Int.Map.t

val mk_abs_node :
  ?bud:bool -> Tags.t -> int list -> (Tagpairs.t * Tagpairs.t) list -> abstract_node
(** Constructor for nodes. *)

val build_proof :
  (int * int list * (int * (int * int) list * (int * int) list) list) list -> t
(* Make an abstract proof from a representation using integers for node and tag
   IDs. The representation consists of a list of tuples, each representing one
   node of the proof and consisting of:

     - a node ID
     - a list of the tag IDs belonging to that node
     - a list of tuples representing the successors of the node, each consisting
       of:
         # the ID of the successor
         # a list of valid tag transitions
         # a list of the progressing tag transitions
 *)

val use_legacy : unit -> unit
(** Flag to indicate that the legacy encoding using Spot model checker should be
    used to verify the trace condition for proofs. *)
val use_vla : unit -> unit
(** Flag to indicate whether the Spot model checker should be used to verify the
    trace condition for proofs, using the vertex-language encoding. *)
val use_sla : unit -> unit
(** Flag to indicate whether the Spot model checker should be used to verify the
    trace condition for proofs, using the slope-language encoding. *)

val use_ortl : unit -> unit
(** Flag to indicate whether trace condition check should be done by the
    order-reduced relational method using external C++ code. *)
val set_node_order : int -> unit
(** Specify which node order to use in the order-reduced relational method. *)

val use_fwk : unit -> unit
(** Flag to indicate whether trace condition check should be done by the
    Floyd-Warshall-Kleene relational method using external C++ code. *)

val fail_fast : unit -> unit
(** Set fail_fast flag for C++ relation-based trace condition check *)
val use_scc_check : unit -> unit
(** Set use_scc_check flag for C++ relation-based trace condition check *)
val use_idempotence : unit -> unit
(** Set use_idempotence flag for C++ relation-based trace condition check *)
val use_minimality : unit -> unit
(** Set use_minimality flag for C++ relation-based trace condition check *)

val arg_opts : (string * Arg.spec * string) list
(* Specification of command-line options applying to soundness check *)

val check_proof : ?init:int -> ?minimize:bool -> t -> bool
(** Validate, minimise, check soundness of proof/graph and memoise. *)

(** Module type for concrete representations of abstract proofs *)
module type Representation = sig
  val pp : Format.formatter -> t -> unit
  (** Pretty print abstract proof. *)
  val to_string : t -> string
  (** Convert abstract proof to string *)
  val parse : (t, 's) MParser.parser
  (** Parse an abstract proof from the concrete representation *)
end

(* Concrete representations for abstract proofs *)

module NodeList : Representation
module EdgeList : Representation
module JSON : Representation

(** Parse and pretty print functions from the representation selected by the
    command line arguments. *)
include Representation