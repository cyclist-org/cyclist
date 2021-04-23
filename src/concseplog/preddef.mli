include Lib.BasicType

val mk : Indrule.t list * Predsym.t -> t

val dest : t -> Indrule.t list * Predsym.t

val predsym : t -> Predsym.t

val rules : t -> Indrule.t list

val parse : (t, 'a) MParser.t

val memory_consuming : t -> bool

val constructively_valued : t -> bool

val deterministic : t -> bool
(** [deterministic p] checks if all rules defining [p] are pairwise inconsistent
    when projected down to their formal parameters via [Heap.project]. **)
