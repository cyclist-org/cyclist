val do_check : Soundcheck.t -> unit
val process_files : (Soundcheck.t list, unit) MParser.t -> string list -> unit
val process_stdin : (Soundcheck.t list, unit) MParser.t -> bool -> 'a

val cmd : unit Cmdliner.Cmd.t
(** The [cyclist checkproof] subcommand. *)
