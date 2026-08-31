val overall_circuit_def : int -> string
val overall_bitvector_def : int -> string

val cmd : unit Cmdliner.Cmd.t
(** The [cyclist sl satexpgen] subcommand. *)
