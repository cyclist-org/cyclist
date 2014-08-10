include Util.OrderedContainer with type elt = Sl_pto.t
val parse : (Sl_pto.t, 'a) MParser.parser
val subst : Sl_term.substitution -> t -> t
val terms : t -> Sl_term.Set.t
val vars : t -> Sl_term.Set.t
val to_string_list : t -> string list
val to_melt : t -> Latex.t
val subsumed : Sl_uf.t -> t -> t -> bool
val unify : t Sl_term.gen_unifier
val unify_with_part : t Sl_term.gen_unifier
