exception Not_symheap

module Rule :
  Generic.Proofrule.S
    with type seq_t = Program.Seq.t
     and type proof_t = Generic.Proof.Make(Program.Seq).t

module Seqtactics : Generic.Seqtactics.S with type seq_t = Program.Seq.t

module Abdrule :
  Generic.Abdrule.S
    with type seq_t = Program.Seq.t
     and type proof_t = Generic.Proof.Make(Program.Seq).t
     and type rule_t = Generic.Proofrule.Make(Program.Seq).t
     and type defs_t = Seplog.Defs.t

val dest_sh_seq :
  Seplog.Form.t * 'a -> (Generic.Ord_constraints.t * Program.SH.symheap) * 'a

val last_pred : int ref
val get_fresh_ident : unit -> Seplog.Predsym.t
val get_undefined : Seplog.Defs.t -> Program.SH.symheap -> Seplog.Tpreds.t
val ex_subst_defs : Seplog.Defs.t -> Seplog.Defs.t
val empify : Seplog.Defs.t -> Seplog.Defs.t
val inline : Seplog.Defs.t -> Seplog.Defs.t

val used_only_recursively :
  Program.SH.symheap * (Seplog.Predsym.t * Seplog.Term.t list) -> int -> bool

val find_unused_arg : Seplog.Defs.t -> (Seplog.Predsym.t * int) option
val eliminate : Seplog.Predsym.t * int -> Seplog.Defs.t -> Seplog.Defs.t
val elim_dead_vars : Seplog.Defs.t -> Seplog.Defs.t
val simplify_defs : Seplog.Defs.t -> Seplog.Defs.t
val is_sat : Seplog.Defs.t -> bool
val ex_falso_axiom : Abdrule.t
val symex_empty_axiom : Abdrule.t
val lhs_disj_to_symheaps : Abdrule.t

val eq_subst_ex :
  Seplog.Form.t * 'a ->
  (((Seplog.Form.t * 'a) * Generic.Tagpairs.t * Generic.Tagpairs.t) list
  * string)
  list

val simpl_deqs :
  Seplog.Form.t * 'a ->
  ((((Generic.Ord_constraints.t * Program.SH.symheap list) * 'a)
   * Generic.Tagpairs.t
   * Generic.Tagpairs.t)
   list
  * string)
  list

val simplify : Abdrule.t
val wrap : Abdrule.t -> Abdrule.t
val symex_stop_axiom : Abdrule.t
val symex_load_rule : Abdrule.t
val symex_store_rule : Abdrule.t
val symex_free_rule : Abdrule.t
val symex_new_rule : Abdrule.t
val symex_skip_rule : Abdrule.t
val symex_assign_rule : Abdrule.t
val symex_nondet_if_rule : Abdrule.t
val symex_nondet_ifelse_rule : Abdrule.t
val symex_nondet_while_rule : Abdrule.t
val symex_det_if_rule : Abdrule.t
val symex_det_ifelse_rule : Abdrule.t
val symex_det_while_rule : Abdrule.t
val generalise_while_rule : Abdrule.t
val abd_deref : Abdrule.t
val abd_det_guard : Abdrule.t
val abd_back_rule : Abdrule.t
val matches : Abdrule.t
val unfold : Abdrule.t
val unfold_last : Abdrule.t
val deref_tac : Abdrule.t
val det_guard_tac : Abdrule.t
val abd_symex : Abdrule.t -> Abdrule.t -> Abdrule.t
val ifwhile_tac : Abdrule.t
val gen_ifwhile_tac : Abdrule.t
val rules : Abdrule.t
