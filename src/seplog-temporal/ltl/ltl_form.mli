open Lib
open Generic
open Seplog

type t =
  | Atom of Sl_heap_rho.t * int
  | Next of t * int
  | G of Tags.elt * t * int
  | F of t * int
  | And of t list * int
  | Or of t list * int
include BasicType with type t:=t

val is_atom : t -> bool
val is_next : t -> bool
val is_g : t -> bool
val is_f : t -> bool
val is_and : t -> bool
val is_or : t -> bool
val is_slformula : t -> bool
val is_checkable : t -> bool

val dest_atom : t -> Sl_heap_rho.t
val dest_next : t -> t
val dest_g : t -> Tags.elt * t
val dest_f : t -> t
val dest_and : t -> t list
val dest_or : t -> t list

val mk_atom : Sl_heap_rho.t -> t
val mk_next : t -> t
val mk_g : Tags.elt -> t -> t
val mk_f : t -> t
val mk_and : t list -> t
val mk_or : t list -> t

val unfold_g : t -> t * t
val unfold_f : t -> t * t

val unfold_or : t -> t * t
val unfold_and : t -> t * t

val to_string : t -> string

val parse : (t, 'a) MParser.t
val of_string : string -> t

val to_slformula : t -> Sl_form_rho.t
val extract_checkable_slformula : t -> Sl_form_rho.t
val tags : t -> Tags.t
val outermost_tag : t -> Tags.t
val subst_tags : Tagpairs.t -> t -> t
val complete_tags : Tags.t -> t -> t
val vars : t -> Term.Set.t
val equal : t -> t -> bool
val equal_upto_tags : t -> t -> bool

val step : t -> t
