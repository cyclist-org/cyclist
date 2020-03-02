open Lib
open Generic

type t

val mk : Prod.t -> Term.t list -> t
val dest : t -> Prod.t * Term.t list
val vars : t -> Term.Set.t
val freshen : Term.Set.t -> t -> t

val to_string : string -> t -> string

val parse_case : (t * string, 'a) MParser.t
val parse_cases : ((t * string) list, 'a) MParser.t
