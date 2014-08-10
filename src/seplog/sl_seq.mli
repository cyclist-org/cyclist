(** SL sequent, as a pair of SL formulas. *)

include Util.BasicType with type t = Sl_form.t * Sl_form.t

val equal_upto_tags : t -> t -> bool

val dest : t -> Sl_heap.t * Sl_heap.t
(** If both LHS and RHS are symbolic heaps then return them else raise
    [Sl_form.Not_symheap]. *)

val parse : (t, 'a) MParser.t
val of_string : string -> t

val to_melt : t -> Latex.t

val vars : t -> Sl_term.Set.t

val tags : t -> Util.Tags.t
(** Only LHS tags are returned. *)

val tag_pairs : t -> Util.TagPairs.t
(** Only LHS tag pairs are returned. *)

val subst_tags : Util.TagPairs.t -> t -> t
(** Substitute tags of the LHS. *)

val subst : Sl_term.substitution -> t -> t

val subsumed : t -> t -> bool

