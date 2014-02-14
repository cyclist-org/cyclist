(** Provides an abstract view of a proof as a graph and allows checking its 
    soundness. *)

(** Abstract proof node type. The only 
    information stored is a set of tags (integers) and a list of
    tuples of: successor, set of valid tag transitions and set of
    progressing tag transitions. *) 
type abstract_node 

(** Constructor for nodes. *)
val mk_abs_node :   
  Util.Tags.t -> ((int * Util.TagPairs.t* Util.TagPairs.t ) list) -> 
    abstract_node 

(** The type of abstracted proof as a map from ints to nodes. *)
type t = abstract_node Util.Int.Map.t

(** Validate, minimise, check soundness of proof/graph and memoise. *)
val check_proof : t -> bool

(** Pretty print abstract proof. *)
val pp : Format.formatter -> t -> unit
