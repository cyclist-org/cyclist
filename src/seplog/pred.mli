(** Predicate occurrences consisting of a predicate identifier and a list of 
    terms as parameters. *)

open Lib

include BasicType with type t = Predsym.t * Term.FList.t

module MSet : OrderedContainer with type elt = t

val predsym : t -> Predsym.t

val arity : t -> int

val args : t -> Term.t list

val terms : t -> Term.Set.t

val vars : t -> Term.Set.t

val subst : Subst.t -> t -> t

val parse : (t, 'a) MParser.parser

val of_string : string -> t

val unify :
     ?update_check:Unify.Unidirectional.update_check
  -> t Unify.Unidirectional.unifier
(** Compute substitution that unifies two predicates. *)

val biunify :
     ?update_check:Unify.Bidirectional.update_check
  -> t Unify.Bidirectional.unifier

val norm : Uf.t -> t -> t
(** Replace all terms with their UF representative. NB this may replace [nil] 
    with a variable. *)
