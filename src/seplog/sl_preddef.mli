include Utilsigs.BasicType

val mk : Sl_indrule.t list * Sl_predsym.t -> t

val dest : t -> Sl_indrule.t list * Sl_predsym.t

val predsym : t -> Sl_predsym.t

val rules : t -> Sl_indrule.t list

val parse : (t, 'a) MParser.t

val memory_consuming : t -> bool

val constructively_valued : t -> bool

val deterministic : t -> bool
(** [deterministic p] checks if all rules defining [p] are pairwise inconsistent
    when projected down to their formal parameters via [Sl_heap.project]. **)
