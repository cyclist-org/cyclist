include Util.BasicType with 
  type t = (Sl_indrule.t list * Sl_pred.ident_t) list
  
    
val to_melt : t -> Latex.t

val mem : Sl_pred.ident_t -> t -> bool
val is_defined : t -> Sl_tpred.t -> bool
val is_undefined : t -> Sl_tpred.t -> bool
val get_def : Sl_pred.ident_t -> t -> Sl_indrule.t list

val satisfiable : t -> bool -> bool -> bool

val fixpoint: (t -> t) -> t -> t

val parse : (t, 'a) MParser.t
val of_channel : in_channel -> t

val unfold : Sl_term.Set.t -> Sl_heap.t -> Sl_tpred.t -> t -> 
  (Sl_heap.t * Util.TagPairs.t) list
