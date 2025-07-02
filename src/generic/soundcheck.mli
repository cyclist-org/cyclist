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

val use_vla : unit -> unit
(** Use the Spot model checker to verify the infinite descent trace condition
    for proofs, with the vertex-language encoding. *)
val use_sla : unit -> unit
(** Use the Spot model checker to verify the infinite descent trace condition
    for proofs, with the slope-language encoding. *)
val use_fwk_order_reduced : unit -> unit
(** Use the C++ order reduced Floyd-Warshall-Kleene method to verify the
    infinite descent trace condition for proofs. This is the default method. *)
val use_fwk_full : unit -> unit
(** Use the C++ standard Floyd-Warshall-Kleene method to verify the infinite
    descent trace condition for proofs. *)
val use_ocaml : unit -> unit
(** Use the OCaml implementation of the infinite descent check to verify the
    trace condition for proofs. *)
val use_legacy : unit -> unit
(** Use the Spot model checker to verify the infinite descent trace condition
    for proofs, with the legacy vertex-language encoding. *)

val set_node_order : int -> unit
(** Specify which node order to use in the order-reduced relational method:
      0 - Natural Ordering
      1 - Out-degree, In-degree (lexicographically) ascending
      2 - Out-degree, In-degree (lexicographically) descending
*)

val fail_fast : bool -> unit
(** Specify whether the Floyd-Warshall-Kleene infinite descent checks should
    eagerly check for failure or not.
    Default is [true]. *)
val use_minimality : bool -> unit
(** Specify whether the Floyd-Warshall-Kleene infinite descent checks should
    use the subsumption-based minimality optimisation or not.
    This optimisation is enabled by default.
    Note that this optimisation is not compatible with using the idempotent
    looping check, so if this is the selected loop check method then calling
    [use_minimality true] will raise an [Invalid_argument] exception. *)
val use_transitive_loop_check : unit -> unit
(** Specify whether the Floyd-Warshall-Kleene infinite descent checks should
    use the transitive looping check. This is the default. *)
val use_idempotent_loop_check : unit -> unit
(** Specify whether the Floyd-Warshall-Kleene infinite descent checks should
    use the idempotent looping check. This is not compatible with the
    subsumption-based minimality optimisation, and so calling this function
    will disable this optimisation. *)

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