include Lib.BasicType

val parse : (t, 'a) MParser.t

(** Constructors *)

val mk_atom : string -> t
val mk_negatom : string -> t
val mk_disj : t * t -> t
val mk_conj : t * t -> t
val mk_next : t -> t
val mk_eventually : t -> t
val mk_always : t -> t

module Operators :
sig
  val at : string -> t
  val ( || ) : t -> t -> t
  val ( && ) : t -> t -> t
  val nxt : t -> t
  val ev : t -> t
  val alw : t -> t

  val _X : t -> t
  val _F : t -> t
  val _G : t -> t
end

(** Destructors *)

val dest_atom : t -> string option
val dest_negatom : t -> string option
val dest_disj : t -> (t * t) option
val dest_conj : t -> (t * t) option
val dest_next : t -> t option
val dest_eventually : t -> t option
val dest_always : t -> t option

(** Operations *)

val neg : t -> t

(** Predicates *)

val is_disj : t -> bool
val is_conj : t -> bool
val is_next : t -> bool
val is_eventually : t -> bool
val is_always : t -> bool

val is_traceable : t -> bool