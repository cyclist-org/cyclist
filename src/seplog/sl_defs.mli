include Util.BasicType

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

val unfold : (Sl_term.Set.t * Util.Tags.t) -> Sl_tpred.t -> t -> Sl_heap.t list
(** [unfold (vs, ts) p defs] returns a list containing the bodies of all the 
    inductive rules for [p] in [defs] where, for each rule body:
      the formal parameters have been replaced by the arguments of [p]; 
      the remaining variables have been freshened, avoiding those in [vs]; and
      the predicates have been assigned fresh existential tags, avoiding those in [ts]
    NB. This assumes that all predicates in the rules bodies are untagged.
*)

val of_formula : t -> Sl_form.t -> t
(** Convert a formula to a set of rules and add it to the provided set of
    definitions.  The head of the return definition list contains these rules. *)

val memory_consuming : t -> bool
val constructively_valued : t -> bool
val deterministic : t -> bool
