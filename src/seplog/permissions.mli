(** Type representing permissions (for Concurrent Separation Logic automation) *)


open Lib
open Generic

type permFraction = private
  { num: int
  ; den: int}

include BasicType with type t = permFraction

(* Constructors *)
val one : t

val zero: t
 
(* Basic arithmetics *)
 

val half: t -> t
val third: t -> t
val splitith: t -> int -> t

val add: t -> t -> t
  