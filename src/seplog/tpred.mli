(** Tagged predicate, as a pair of an integer and a predicate. *)
open Lib
open Generic 

(* include BasicType with type t = Tags.Elt.t * Pred.t *)
include BasicType with type t = (Permission.t * Tags.Elt.t) * Pred.t

val equal_upto_tags : t -> t -> bool
(** Compare for equality two tagged predicates while ignoring tags. *)

val subst : Subst.t -> t -> t

val subst_tag : Tagpairs.t -> t -> t
(** Substitute the tag according to the function represented by the set of
    tag pairs provided. *)

val predsym : t -> Predsym.t

val tag : t -> Tags.Elt.t

val arity : t -> int

val args : t -> Term.t list

val tag : t -> Tags.Elt.t

val tags : t -> Tags.t

val terms : t -> Term.Set.t

val vars : t -> Term.Set.t

val is_tagged : t -> bool

val tag_is_free : t -> bool

val tag_is_exist : t -> bool

val to_string : t -> string

val parse : ?allow_tags:bool -> (t, 'a) MParser.parser

val of_string : string -> t

val unify :
     ?tagpairs:bool
  -> ?update_check:Unify.Unidirectional.update_check
  -> t Unify.Unidirectional.unifier
(** Unify two tagged predicates.
    If the optional argument [~tagpairs=false] is set to [true] then also
    add the pair of tags of the unified predicates. *)

val biunify :
     ?tagpairs:bool
  -> ?update_check:Unify.Bidirectional.update_check
  -> t Unify.Bidirectional.unifier

val norm : Uf.t -> t -> t
(** Replace all terms with their UF representative. NB this may replace [nil]
    with a variable. *)
