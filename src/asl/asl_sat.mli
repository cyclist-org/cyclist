val z3_path : string ref
(** Path to the z3 executable, looked up in PATH by default. *)

val timeout : int ref
(** Timeout, in seconds, of an individual call to z3. *)

val times : int ref
val check_satisfiability : string -> string
val is_sat : string -> bool
val is_unsat : string -> bool
