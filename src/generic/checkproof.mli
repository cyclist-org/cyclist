val allow_comments : bool ref
val input_files : string list ref
val speclist : (string * Arg.spec * string) list
val usage : string
val do_check : Generic.Soundcheck.t -> unit
val process_files : (Generic.Soundcheck.t list, unit) MParser.t -> unit
val process_stdin : (Generic.Soundcheck.t list, unit) MParser.t -> 'a
