include Lib.BasicType

type letter = Char.t
(** the type of letters *)

val parse : (t, 'a) MParser.t
val of_string : string -> t

val letter_to_string : letter -> string

(** Constructors *)

val zero : t
val one : t
val letter : letter -> t
val choice : t -> t -> t
val concat : t -> t -> t
val star : t -> t

val either : t list -> t
val concatenate : t list -> t

module Operators :
  sig
    val ( <.> ) : t -> t -> t
    val ( <+> ) : t -> t -> t
  end

(* Destructors *)

val dest_letter : t -> letter option
val dest_choice : t -> (t * t) option
val dest_concat : t -> (t * t) option
val dest_star : t -> t option

(* Predicates *)

val is_zero : t -> bool
val is_one : t -> bool
val is_letter : t -> bool
val is_choice : t -> bool
val is_concat : t -> bool
val is_star : t -> bool

val is_atom : t -> bool

val contains_empty : t -> bool
val can_start_with : letter -> t -> bool

(* Operations *)

(* Split apart formula into list of top-level summands *)
val partition : t -> t list

(* Split apart formula into list of top-level factors *)
val factorise : t -> t list
