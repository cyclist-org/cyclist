type state = { lhs : Heap.t option; rhs : Heap.t option; defs : Preddef.t list }

val initial : state
val skip_until : char -> (unit, 'a) MParser.t
val skip_comment : (unit, state) MParser.t
val space : (unit, state) MParser.t
val spaces : (unit list, state) MParser.t
val spaces1 : (unit list, state) MParser.t
val parens : ('a, state) MParser.t -> ('a, state) MParser.t
val skip_label : string -> (unit, 'a) MParser.t
val skip_brexp : (unit, state) MParser.t
val skip_ident : (unit, state) MParser.t
val skip_types : (unit, state) MParser.t
val skip_heap : (unit, state) MParser.t
val parse_exp : (Term.t, state) MParser.t
val parse_pred : (Heap.t, state) MParser.t
val parse_emp : (Heap.t, state) MParser.t
val parse_eq : (Heap.t, state) MParser.t
val parse_pto : (Heap.t, state) MParser.t
val parse_deq : (Heap.t, state) MParser.t

val parse_binop :
  string -> (Heap.t -> Heap.t -> Heap.t) -> (Heap.t, state) MParser.t

val parse_and : (Heap.t, state) MParser.t
val parse_sep : (Heap.t, state) MParser.t
val parse_exists : (Heap.t, state) MParser.t
val parse_heap : (Heap.t, state) MParser.t
val parse_or : (Heap.t list, state) MParser.t
val parse_body : (Heap.t list, state) MParser.t
val parse_def : (unit, state) MParser.t
val parse_def_preds : ((Predsym.t * Term.t list) list, state) MParser.t
val parse_bodies : (Heap.t list list, state) MParser.t
val parse_defs : (unit, state) MParser.t
val parse_lhs : (unit, state) MParser.t
val parse_rhs : (unit, state) MParser.t
val skip_item : (unit, state) MParser.t
val parse_item : (unit, state) MParser.t
val parse_seq_part : (unit, state) MParser.t
val parse_entl : (state, state) MParser.t
val of_channel : in_channel -> (Form.t * Form.t) * Defs.t
val parse_sat : (state, state) MParser.t
val defs_of_channel : in_channel -> Defs.t * Form.t
