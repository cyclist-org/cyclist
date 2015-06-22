include Util.BasicType 

val mk : Sld_indrule.t list * Sld_predsym.t -> t
val dest : t -> Sld_indrule.t list * Sld_predsym.t

val predsym : t -> Sld_predsym.t
val rules : t -> Sld_indrule.t list

val parse : (t, 'a) MParser.t
