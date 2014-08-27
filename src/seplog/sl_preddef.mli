include Util.BasicType 

val mk : Sl_indrule.t list * Sl_predsym.t -> t
val dest : t -> Sl_indrule.t list * Sl_predsym.t

val predsym : t -> Sl_predsym.t
val rules : t -> Sl_indrule.t list

val parse : (t, 'a) MParser.t