include Util.BasicType 

val mk : Sl_indrule.t list * Sl_pred.ident_t -> t
val dest : t -> Sl_indrule.t list * Sl_pred.ident_t

val predsym : t -> Sl_pred.ident_t
val rules : t -> Sl_indrule.t list

val parse : (t, 'a) MParser.t