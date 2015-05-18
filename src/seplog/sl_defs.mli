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

val rule_fold : ('a -> Sl_indrule.t -> 'a) -> 'a -> t -> 'a 

val parse : (t, 'a) MParser.t
val of_channel : in_channel -> t
val of_string : string -> t

val unfold : Sl_term.Set.t -> Sl_heap.t -> Sl_tpred.t -> t -> 
  (Sl_heap.t * Util.TagPairs.t) list

val of_formula : t -> Sl_form.t -> t
(** Convert a formula to a set of rules and add it to the provided set of 
    definitions.  The head of the return definition list contains these rules. *)