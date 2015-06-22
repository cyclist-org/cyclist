include Util.BasicType
  
val empty : t
val add : Sld_preddef.t -> t -> t
val to_list : t -> Sld_preddef.t list
val of_list : Sld_preddef.t list -> t
                        
val to_melt : t -> Latex.t

val mem : Sld_predsym.t -> t -> bool
val is_defined : t -> Sld_tpred.t -> bool
val is_undefined : t -> Sld_tpred.t -> bool
val get_def : Sld_predsym.t -> t -> Sld_indrule.t list

val fixpoint: (t -> t) -> t -> t

val relevant_defs : t -> Sld_form.t -> t

val check_form_wf : t -> Sld_form.t -> unit

val rule_fold : ('a -> Sld_indrule.t -> 'a) -> 'a -> t -> 'a 
val rule_iter : (Sld_indrule.t -> unit) -> t -> unit

val parse : (t, 'a) MParser.t
val of_channel : in_channel -> t
val of_string : string -> t

val unfold : Sld_term.Set.t -> Sld_heap.t -> Sld_tpred.t -> t -> 
  (Sld_heap.t * Util.TagPairs.t) list

val of_formula : t -> Sld_form.t -> t
(** Convert a formula to a set of rules and add it to the provided set of 
    definitions.  The head of the return definition list contains these rules. *)
