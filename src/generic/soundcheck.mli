(** Provides an abstract view of a proof as a graph and allows checking its 
    soundness. *)

open Lib

(** Abstract proof node type. The only 
    information stored is a set of tags (integers) and a list of
    tuples of: successor, set of valid tag transitions and set of
    progressing tag transitions. *)
type abstract_node

val use_spot : bool ref
(** Flag to indicate whether the Spot model checker should be used to verify the
    trace condition for proofs. *)

val mk_abs_node :
  Tags.t -> int list -> (Tagpairs.t * Tagpairs.t) list -> abstract_node
(** Constructor for nodes. *)

(** The type of abstracted proof as a map from ints to nodes. 
    NB the root is always at 0. *)
type t = abstract_node Int.Map.t

val build_proof :
  (int * int list * (int * (int * int) list * (int * int) list) list) list -> t

val check_proof : ?init:int -> t -> bool
(** Validate, minimise, check soundness of proof/graph and memoise. *)

val pp : Format.formatter -> t -> unit
(** Pretty print abstract proof. *)
