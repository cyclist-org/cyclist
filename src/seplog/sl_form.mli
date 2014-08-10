(** SL formula, basically a list of symbolic heaps, denoting their disjunction. 
    NB no attempt to enforce variable or tag hygiene is made.  For example,
    if [f] and [g] both use an existential variable [x'] then [[f;g]] would
    mean the bound variable is shared. *)

include Util.BasicType with type t = Sl_heap.t list

val empty : t
(** The formula [emp]. NB this is not the same as [[]], which is equivalent to 
    false. *)

exception Not_symheap
val dest : t -> Sl_heap.t
(** Return the single disjunct, if there is exactly one, else raise [Not_symheap]. *)

val equal_upto_tags : t -> t -> bool

val to_melt : t -> Latex.t
val terms : t -> Sl_term.Set.t
val vars : t -> Sl_term.Set.t

val tags : t -> Util.Tags.t
(** NB no attempt is made to ensure that tags are disjoint between disjuncts. 
    This is not necessarily unsound. *)
val tag_pairs : t -> Util.TagPairs.t
(** The proviso on tags applies here too. *)

val inconsistent : t -> bool
(** Do all disjuncts entail false in the sense of [Sl_heap.inconsistent]? *)

val subsumed : t -> t -> bool
(** [subsumed a b]: is it the case that for any disjunct [a'] of [a] there a 
    disjunct [b'] of [b] such [a'] is subsumed by [b']? *)   

val parse : (t, 'a) MParser.t
val of_string : string -> t

val star : t -> t -> t
(** Star two formulas by distributing [*] through [\/]. *)

val disj : t -> t -> t
(** Or two formulas (list-append). *)

val subst : Sl_term.substitution -> t -> t
val subst_existentials : t -> t
val subst_tags : Util.TagPairs.t -> t -> t

