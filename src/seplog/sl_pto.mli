include Util.BasicType with type t = Sl_term.t * Sl_term.FList.t

val subst : Sl_term.substitution -> t -> t
val unify : t Sl_term.unifier
val to_melt : t -> Latex.t
val terms : t -> Sl_term.Set.t
val vars : t -> Sl_term.Set.t