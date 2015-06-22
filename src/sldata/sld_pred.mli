(** Predicate occurrences consisting of a predicate identifier and a list of 
    terms as parameters. *)
    
include Util.BasicType with type t = Sld_predsym.t * Sld_term.FList.t
module MSet : Util.OrderedContainer with type elt = t

val predsym : t -> Sld_predsym.t
val arity : t -> int
val args : t -> Sld_term.t list

val terms : t -> Sld_term.Set.t
val vars : t -> Sld_term.Set.t

val subst : Sld_term.substitution -> t -> t
val parse : (t, 'a) MParser.parser
val of_string : string -> t

val unify : t Sld_term.unifier
(** Compute substitution that unifies two predicates. *)

val norm : Sld_uf.t -> t -> t
(** Replace all terms with their UF representative. NB this may replace [nil] 
    with a variable. *)
    
