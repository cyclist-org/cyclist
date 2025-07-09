open Lib
open Generic
open Seplog

open MParser

type t =
  | Final of int
  | Atom of Heap.t * int
  | Circle of t * int
  | Diamond of t * int
  | Box of t * int
  | AG of Tags.elt * t * int
  | EG of Tags.elt * t * int
  | AF of t * int
  | EF of t * int
  | And of t list * int
  | Or of t list * int

include BasicType with type t:=t

val is_final : t -> bool
val is_atom : t -> bool
val is_circle : t -> bool
val is_diamond : t -> bool
val is_box : t -> bool
val is_ag : t -> bool
val is_eg : t -> bool
val is_af : t -> bool
val is_ef : t -> bool
val is_and : t -> bool
val is_or : t -> bool
val is_slformula : t -> bool
val is_checkable : t -> bool

val dest_atom : t -> Heap.t
val dest_circle : t -> t
val dest_diamond : t -> t
val dest_box : t -> t
val dest_ag : t -> Tags.elt * t
val dest_eg : t -> Tags.elt * t
val dest_af : t -> t
val dest_ef : t -> t
val dest_and : t -> t list
val dest_or : t -> t list

val mk_final : t
val mk_atom : Heap.t -> t
val mk_circle : t -> t
val mk_diamond : t -> t
val mk_box : t -> t
val mk_ag : Tags.elt -> t -> t
val mk_eg : Tags.elt -> t -> t
val mk_af : t -> t
val mk_ef : t -> t
val mk_and : t list -> t
val mk_or : t list -> t

val unfold_ag : t -> t * t
val unfold_eg : t -> t * t
val unfold_af : t -> t * t
val unfold_ef : t -> t * t

val unfold_or : t -> t * t
val unfold_and : t -> t * t

val to_string : t -> string

val parse : (t, 'a) MParser.t
val of_string : string -> t

(* val norm : t -> t *)
val to_slformula : t -> Form.t
val extract_checkable_slformula : t -> Form.t
val tags : t -> Tags.t
val outermost_tag : t -> Tags.t
val subst_tags : Tagpairs.t -> t -> t
val vars : t -> Term.Set.t
val equal : t -> t -> bool
val equal_upto_tags : t -> t -> bool

val complete_tags : Tags.t -> t -> t
(** [complete_tags ts f] returns the formula obtained from f by assigning
    all untagged AG/EG occurrences a fresh free tag, avoiding those in [ts].
*)

val e_step : t -> t
val a_step : t -> t
