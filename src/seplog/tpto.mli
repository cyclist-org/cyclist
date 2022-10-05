open Lib
open Generic 
include BasicType with type t = (Tags.Elt.t * Permission.t) * Pto.t


val subst : Subst.t -> t -> t

val terms : t -> Term.Set.t

val vars : t -> Term.Set.t

(* val parse : (t, 'a) MParser.parser *)
