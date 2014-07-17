include Util.BasicType with type t = Sl_form.t * Sl_form.t

val dest : t -> Sl_heap.t * Sl_heap.t
val to_melt : t -> Latex.t
val vars : t -> Sl_term.Set.t
val tags : t -> Util.Tags.t
val tag_pairs : t -> Util.TagPairs.t
val subst : Sl_term.substitution -> t -> t
val subsumed_wrt_tags : Util.Tags.t -> t -> t -> bool
val uni_subsumption : t -> t -> Sl_term.substitution option
val norm : t -> t
val parse : (t, 'a) MParser.t
val of_string : string -> t
