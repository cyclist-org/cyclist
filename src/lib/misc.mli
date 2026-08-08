val do_debug : bool ref
val debug : (unit -> string) -> unit
val run_identifier : string ref
val genhash : int -> int -> int
val pp_comma : Format.formatter -> unit -> unit
val pp_semicolonsp : Format.formatter -> unit -> unit
val pp_commasp : Format.formatter -> unit -> unit
val pp_dbl_nl : Format.formatter -> unit -> unit
val pp_star : Format.formatter -> unit -> unit
val mk_to_string : (Format.formatter -> 'a -> unit) -> 'a -> string

module HashtablePrinter : sig
  module type S = sig
    type 'a t
    type key

    val pp :
      (Format.formatter -> key -> unit) ->
      (Format.formatter -> 'a -> unit) ->
      Format.formatter ->
      'a t ->
      unit

    val to_string :
      (Format.formatter -> key -> unit) ->
      (Format.formatter -> 'a -> unit) ->
      'a t ->
      string
  end

  module Make (H : Hashtbl.S) : S with type 'a t := 'a H.t and type key := H.key
end

val fixpoint : ('a -> 'a -> bool) -> ('a -> 'a) -> 'a -> 'a
val bracket : string -> string
val sqbracket : string -> string
val gc_setup : unit -> unit

exception Timeout

val sigalrm_handler : Sys.signal_behavior
val w_timeout : (unit -> 'a) -> int -> 'a option
val rexp : MParser_RE.Regexp.t
val parse_ident : (string, 'a) MParser.t
val handle_reply : 'a MParser.result -> 'a
val runtest : string -> (unit -> unit) -> unit
val mk_of_string : ('a, unit) MParser.t -> string -> 'a
