(** SL sequent, as a pair of SL formulas. *)

include Utilsigs.BasicType with type t = Sl_form.t * Sl_form.t

val equal_upto_tags : t -> t -> bool
(** Like [equal] but ignoring LHS tags as well as RHS ones. *)

val dest : t -> (Ord_constraints.t * Sl_heap.t) * (Ord_constraints.t * Sl_heap.t)
(** If both LHS and RHS are symbolic heaps then return them else raise
    [Sl_form.Not_symheap]. *)

val parse : ?null_is_emp:bool -> (t, 'a) MParser.t
val of_string : ?null_is_emp:bool -> string -> t

val to_melt : t -> Latex.t

val vars : t -> Sl_term.Set.t

val tags : t -> Tags.t
(** Tags occurring in this sequent on both the LHS and RHS *)

val tag_pairs : t -> Tagpairs.t
(** Tag pairs constituting the identity relation on the tags in the LHS. *)

val get_tracepairs : t -> t -> (Tagpairs.t * Tagpairs.t)
(** Get the tracepairs between two sequents *)

val subst_tags : Tagpairs.t -> t -> t
(** Substitute tags of the LHS. *)

val subst : Sl_subst.t -> t -> t

val subsumed : t -> t -> bool
(** [subsumed (l,r) (l',r')] is true iff both [Sl_form.subsumed l' l] and
    [Sl_form.subsumed r r'] are true. *)

val subsumed_upto_tags : t -> t -> bool
(** Like [subsumed] but ignoring all tags. *)

val norm : t -> t
(** Replace all terms with their UF representatives in the respective formulas.` *)
