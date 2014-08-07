type ident_t = Util.Strng.t

include Util.BasicType with type t = ident_t * Sl_term.FList.t
module MSet : Util.OrderedContainer with type elt = t

val unify : t Sl_term.unifier
val terms : t -> Sl_term.Set.t
val vars : t -> Sl_term.Set.t
val subst : Sl_term.substitution -> t -> t
val parse : (t, 'a) MParser.parser
