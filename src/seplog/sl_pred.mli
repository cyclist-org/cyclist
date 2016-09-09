(** Predicate occurrences consisting of a predicate identifier and a list of 
    terms as parameters. *)
    
include Utilsigs.BasicType with type t = Sl_predsym.t * Sl_term.FList.t
module MSet : Utilsigs.OrderedContainer with type elt = t

val predsym : t -> Sl_predsym.t
val arity : t -> int
val args : t -> Sl_term.t list

val terms : t -> Sl_term.Set.t
val vars : t -> Sl_term.Set.t

val subst : Sl_subst.t -> t -> t
val parse : (t, 'a) MParser.parser
val of_string : string -> t

val unify : t Sl_unifier.t
(** Compute substitution that unifies two predicates. *)

val norm : Sl_uf.t -> t -> t
(** Replace all terms with their UF representative. NB this may replace [nil] 
    with a variable. *)
    