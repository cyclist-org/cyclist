include Util.OrderedContainer with type elt = Sl_tpred.t

val subst : Sl_term.substitution -> t -> t
val vars : t -> Sl_term.Set.t

val to_string_list : t -> string list
val to_melt : t -> Latex.t

val tags : t -> Util.Tags.t
val freshen_tags : t -> t -> t

val subsumed : Sl_uf.t -> t -> t -> bool

val unify : t Sl_term.gen_unifier
val part_unify : t Sl_term.gen_unifier
