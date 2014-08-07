include Util.BasicType
val mk : Sl_heap.t -> Sl_pred.t -> t
val dest: t -> Sl_heap.t * Sl_pred.t
val vars : t -> Sl_term.Set.t
val freshen : Sl_term.Set.t -> t -> t
val subst : Sl_term.substitution -> t -> t
val parse : (t, 'a) MParser.t
val unfold : 
  Sl_term.Set.t -> Sl_heap.t -> Sl_tpred.t -> t -> 
    (Sl_heap.t * Util.TagPairs.t)
val fold : t -> Sl_heap.t -> Sl_term.substitution list

