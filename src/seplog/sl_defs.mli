include Util.BasicType with 
  type t = (Symheap.Case.t list * Symheap.ind_identifier) list
  
    
val to_melt : t -> Latex.t

val mem : Symheap.ind_identifier -> t -> bool
val is_defined : Symheap.ind_pred -> t -> bool
val get_def : string -> t -> (Symheap.Case.t list * Symheap.ind_identifier)

val satisfiable : t -> bool -> bool -> bool

val fixpoint: (t -> t) -> t -> t

val parse : (t, 'a) MParser.t
val of_channel : in_channel -> t
