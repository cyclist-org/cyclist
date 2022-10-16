open Lib
open Generic 
include BasicType with type t = (Tags.Elt.t * Permission.t) * Pto.t


val subst : Subst.t -> t -> t

val terms : t -> Term.Set.t

val vars : t -> Term.Set.t

val parse :  ?allow_tags:bool -> (t, 'a) MParser.parser

val of_string : string -> t

