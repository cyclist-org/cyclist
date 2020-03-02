open Lib
open Generic

type t

val empty : t
val to_string : t -> string
val pp : Format.formatter -> t -> unit
val add : string -> Case.t -> t -> t
val bindings : t -> (string * Case.t list) list
val of_channel : in_channel -> t
