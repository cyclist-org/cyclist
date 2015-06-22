val partition_strengthening : bool ref
(** Use partition strengthening method when checking invalidity, 
    defaults to [false]. *)

val invalidity_witness : Sld_defs.t -> Sld_seq.t -> Sld_basepair.t option

val check : Sld_defs.t -> Sld_seq.t -> bool
(** Run the base-pairs heuristic to show invalidity. A [false] result does {i not}
    indicated validity. *)

(* val to_z3 : Sld_defs.t -> Sld_seq.t -> unit *)
(** incomplete translation to Z3 *)
