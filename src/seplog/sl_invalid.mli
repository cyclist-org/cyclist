val partition_strengthening : bool ref
(** Use partition strengthening method when checking invalidity, 
    defaults to [false]. *)

val invalidity_witness : Sl_defs.t -> Sl_seq.t -> Sl_basepair.t option

val check : Sl_defs.t -> Sl_seq.t -> bool
(** Run the base-pairs heuristic to show invalidity. A [false] result does {i not}
    indicated validity. *)

(* val to_z3 : Sl_defs.t -> Sl_seq.t -> unit *)
(** incomplete translation to Z3 *)
