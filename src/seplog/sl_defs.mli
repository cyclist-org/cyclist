include Utilsigs.BasicType

val empty : t
val add : Sl_preddef.t -> t -> t
val to_list : t -> Sl_preddef.t list
val of_list : Sl_preddef.t list -> t

val to_melt : t -> Latex.t

val mem : Sl_predsym.t -> t -> bool
val is_defined : t -> Sl_tpred.t -> bool
val is_undefined : t -> Sl_tpred.t -> bool
val get_def : Sl_predsym.t -> t -> Sl_indrule.t list

val fixpoint: (t -> t) -> t -> t

val relevant_defs : t -> Sl_form.t -> t

val check_form_wf : t -> Sl_form.t -> unit
val check_consistency : t -> unit

val rule_fold : ('a -> Sl_indrule.t -> 'a) -> 'a -> t -> 'a
val rule_iter : (Sl_indrule.t -> unit) -> t -> unit

val parse : (t, 'a) MParser.t
val of_channel : in_channel -> t
val of_string : string -> t

val unfold : Sl_term.Set.t -> Sl_heap.t -> Sl_tpred.t -> t ->
  (Sl_heap.t * Tagpairs.t) list

val of_formula : t -> Sl_form.t -> t
(** Convert a formula to a set of rules and add it to the provided set of
    definitions.  The head of the return definition list contains these rules. *)

val memory_consuming : t -> bool
val constructively_valued : t -> bool
val deterministic : t -> bool
