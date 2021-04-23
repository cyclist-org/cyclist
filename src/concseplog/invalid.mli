val partition_strengthening : bool ref
(** Use partition strengthening method when checking invalidity, 
    defaults to [false]. *)

val invalidity_witness : Defs.t -> Seq.t -> Basepair.t option

val check : Defs.t -> Seq.t -> bool
(** Run the base-pairs heuristic to show invalidity. A [false] result does {i not}
    indicated validity. *)

(* val to_z3 : Defs.t -> Seq.t -> unit *)
(** incomplete translation to Z3 *)
