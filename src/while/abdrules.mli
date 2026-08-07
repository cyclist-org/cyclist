exception Not_symheap

module Rule : sig
  type seq_t = Program.Seq.t
  type proof_t = Rules.Rule.proof_t
  type axiom_f = seq_t -> string option

  type infrule_app =
    (seq_t * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string

  type infrule_f = seq_t -> infrule_app list
  type backrule_f = seq_t -> seq_t -> (Generic.Tagpairs.t * string) list
  type select_f = int -> proof_t -> int list
  type t = int -> proof_t -> (int list * proof_t) list

  val mk_axiom : axiom_f -> t
  val mk_infrule : infrule_f -> t
  val mk_backrule : bool -> select_f -> backrule_f -> t
  val all_nodes : select_f
  val closed_nodes : select_f
  val ancestor_nodes : select_f
  val syntactically_equal_nodes : select_f
  val default_select_f : select_f ref
  val set_default_select_f : int -> unit
  val default_select_f_descr : ?line_prefix:string -> unit -> string
  val fail : t
  val identity : t
  val attempt : t -> t
  val compose : t -> t -> t
  val compose_pairwise : t -> t list -> t
  val repeat : t -> t
  val choice : t list -> t
  val first : t list -> t
  val sequence : t list -> t
  val conditional : (seq_t -> bool) -> t -> t
  val combine_axioms : t -> t -> t
end

module Seqtactics : sig
  type seq_t = Rule.seq_t

  type ruleapp_t =
    (seq_t * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string

  type t = seq_t -> ruleapp_t list

  val relabel : string -> t -> t
  val attempt : t -> t
  val compose : t -> t -> t
  val first : t list -> t
  val repeat : t -> t
  val choice : t list -> t
end

module Abdrule : sig
  type seq_t = Rule.seq_t
  type proof_t = Rule.proof_t
  type defs_t = Seplog.Defs.t
  type rule_t = Generic.Proofrule.Make(Program.Seq).t
  type select_f = int -> proof_t -> int list

  type infrule_app =
    (seq_t * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string

  type abdinfrule_f = seq_t -> defs_t -> defs_t list
  type abdbackrule_f = seq_t -> seq_t -> defs_t -> defs_t list
  type abdgenrule_f = seq_t -> defs_t -> (infrule_app * defs_t) list
  type t = int -> proof_t -> defs_t -> ((int list * proof_t) * defs_t) list

  val mk_abdinfrule : abdinfrule_f -> t
  val mk_abdbackrule : select_f -> abdbackrule_f -> t
  val mk_abdgenrule : abdgenrule_f -> t
  val fail : t
  val lift : rule_t -> t
  val compose : t -> t -> t
  val choice : t list -> t
  val attempt : t -> t
  val first : t list -> t
end

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
