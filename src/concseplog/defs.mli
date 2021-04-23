open Lib
open Generic

include BasicType

val empty : t

val add : Preddef.t -> t -> t

val to_list : t -> Preddef.t list

val of_list : Preddef.t list -> t

val mem : Predsym.t -> t -> bool

val is_defined : t -> Tpred.t -> bool

val is_undefined : t -> Tpred.t -> bool

val get_def : Predsym.t -> t -> Indrule.t list

val fixpoint : (t -> t) -> t -> t

val relevant_defs : t -> Form.t -> t

val check_form_wf : t -> Form.t -> unit

val check_consistency : t -> unit

val rule_fold : ('a -> Indrule.t -> 'a) -> 'a -> t -> 'a

val rule_iter : (Indrule.t -> unit) -> t -> unit

val parse : (t, 'a) MParser.t

val of_channel : in_channel -> t

val of_string : string -> t

val unfold :
  ?gen_tags:bool -> Term.Set.t * Tags.t -> Tpred.t -> t -> Heap.t list
(** [unfold (vs, ts) p defs] returns a list containing the bodies of all the
    inductive rules for [p] in [defs] where, for each rule body:
      the formal parameters have been replaced by the arguments of [p];
      the remaining variables have been freshened, avoiding those in [vs]; and
      the predicates have been assigned fresh existential tags avoiding those in [ts],
        unless the optional argument [gen_tags=true] is set to false.
    NB. This assumes that all predicates in the rules bodies are untagged.
*)

val of_formula : t -> Form.t -> t
(** Convert a formula to a set of rules and add it to the provided set of
    definitions.  The head of the return definition list contains these rules. *)

val memory_consuming : t -> bool

val constructively_valued : t -> bool

val deterministic : t -> bool
