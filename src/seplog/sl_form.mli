include Util.BasicType with type t = Symheap.symheap list
val empty : t
val dest : t -> Symheap.Heap.t

val star : t -> t -> t
val disj : t -> t -> t
val to_string : t -> string
val to_melt : t -> Latex.t
val norm : t -> t
val terms : t -> Symheap.Term.Set.t
val vars : t -> Symheap.Term.Set.t
val tags : t -> Util.Tags.t
val tag_pairs : t -> Util.TagPairs.t
val equates : t -> Symheap.Term.t -> Symheap.Term.t -> bool
val inconsistent : t -> bool
val subst : Symheap.Term.substitution -> t -> t
val subsumed_wrt_tags : Util.Tags.t -> t -> t -> bool
val spw_subsumed_wrt_tags : Util.Tags.t -> t -> t -> bool
val right_subsumption :
(Symheap.Term.substitution -> Symheap.Term.substitution option) ->
Symheap.Term.substitution -> t -> t -> Symheap.Term.substitution option
val left_subsumption :
(Symheap.Term.substitution -> Symheap.Term.substitution option) ->
Symheap.Term.substitution -> t -> t -> Symheap.Term.substitution option
(* spatial weakening version *)
val spw_left_subsumption :
(Symheap.Term.substitution -> Symheap.Term.substitution option) ->
Symheap.Term.substitution -> t -> t -> Symheap.Term.substitution option
val subst_existentials : t -> t
val is_fresh_in : Symheap.Term.t -> t -> bool
val is_heap : t -> bool
val parse : (t, 'a) MParser.t
