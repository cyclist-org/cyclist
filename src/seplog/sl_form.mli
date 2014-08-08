include Util.BasicType with type t = Sl_heap.t list
val empty : t
val dest : t -> Sl_heap.t

val star : t -> t -> t
val disj : t -> t -> t
val to_melt : t -> Latex.t
val terms : t -> Sl_term.Set.t
val vars : t -> Sl_term.Set.t
val tags : t -> Util.Tags.t
val tag_pairs : t -> Util.TagPairs.t
val equates : t -> Sl_term.t -> Sl_term.t -> bool
val inconsistent : t -> bool
val subst : Sl_term.substitution -> t -> t
val subsumed : t -> t -> bool
val right_subsumption :
(Sl_term.substitution -> Sl_term.substitution option) ->
Sl_term.substitution -> t -> t -> Sl_term.substitution option
val left_subsumption :
(Sl_term.substitution -> Sl_term.substitution option) ->
Sl_term.substitution -> t -> t -> Sl_term.substitution option
val spw_left_subsumption :
(Sl_term.substitution -> Sl_term.substitution option) ->
Sl_term.substitution -> t -> t -> Sl_term.substitution option
val subst_existentials : t -> t
val is_heap : t -> bool
val parse : (t, 'a) MParser.t
val of_string : string -> t
exception Not_symheap
