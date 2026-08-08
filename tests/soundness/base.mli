val speclist : (string * Arg.spec * string) list
val usage : string
val params : int list ref
val add_param : string -> unit
val runtest : ?minimize:bool -> (unit -> Generic.Soundcheck.t) -> unit
