include Util.BasicType with 
  type t = (Sl_indrule.t list * Symheap.ind_identifier) list
  
    
val to_melt : t -> Latex.t

val mem : Symheap.ind_identifier -> t -> bool
val is_defined : t -> Symheap.ind_pred -> bool
val is_undefined : t -> Symheap.ind_pred -> bool
val get_def : Symheap.ind_identifier -> t -> Sl_indrule.t list

val satisfiable : t -> bool -> bool -> bool

val fixpoint: (t -> t) -> t -> t

val parse : (t, 'a) MParser.t
val of_channel : in_channel -> t

val unfold : Sl_term.Set.t -> Sl_heap.t -> Symheap.ind_pred -> t -> 
  (Sl_heap.t * Util.TagPairs.t) list
