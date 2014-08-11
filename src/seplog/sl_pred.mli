(** Predicate occurrences consisting of a predicate identifier and a list of 
    terms as parameters. *)
    
type ident_t = Util.Strng.t

include Util.BasicType with type t = ident_t * Sl_term.FList.t
module MSet : Util.OrderedContainer with type elt = t

val terms : t -> Sl_term.Set.t
val vars : t -> Sl_term.Set.t
val subst : Sl_term.substitution -> t -> t
val parse : (t, 'a) MParser.parser

val unify : (t, 'a) Sl_term.unifier
(** Compute substitution that unifies two predicates. *)
