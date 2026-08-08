module SH = Seplog.Heap

exception Not_symheap

module Rule :
  Generic.Proofrule.S
    with type seq_t = Program.Seq.t
     and type proof_t = Generic.Proof.Make(Program.Seq).t

module Seqtactics : Generic.Seqtactics.S with type seq_t = Program.Seq.t

module Proof :
  Generic.Proof.S
    with type t = Generic.Proof.Make(Program.Seq).t
     and type seq_t = Program.Seq.t
     and type node_t = Generic.Proofnode.Make(Program.Seq).t

val tagpairs : Seplog.Form.t * 'a -> Generic.Tagpairs.t
val progpairs : unit -> Generic.Tagpairs.t
val dest_sh_seq : Seplog.Form.t * 'a -> (Generic.Ord_constraints.t * SH.t) * 'a
val ex_falso_axiom : Rule.t
val symex_stop_axiom : Rule.t
val symex_empty_axiom : Rule.t

val eq_subst_ex_f :
  Seplog.Form.t * 'a ->
  (((Seplog.Form.t * 'a) * Generic.Tagpairs.t * Generic.Tagpairs.t) list
  * string)
  list

val simplify_rules :
  (Seplog.Form.t * 'a ->
  (((Seplog.Form.t * 'a) * Generic.Tagpairs.t * Generic.Tagpairs.t) list
  * string)
  list)
  list

val simplify_seq_rl : Seqtactics.t
val simplify : Rule.t
val wrap : Seqtactics.t -> Rule.t
val lhs_disj_to_symheaps : Rule.t

val luf_rl :
  Seplog.Form.t * 'a ->
  Seplog.Defs.t ->
  ((((Generic.Ord_constraints.t * SH.t list) * 'a)
   * Generic.Tagpairs.t
   * Generic.Tagpairs.t)
   list
  * string)
  list

val luf : Seplog.Defs.t -> Rule.t

val fix_tps :
  ((Seplog.Form.t * 'a) list * 'b) list ->
  (((Seplog.Form.t * 'a) * Generic.Tagpairs.t * Generic.Tagpairs.t) list * 'b)
  list

val mk_symex :
  (Seplog.Form.t * Program.Cmd.t -> (SH.t list * string) list) -> Rule.t

val symex_assign_rule : Rule.t
val find_pto_on : SH.t -> Seplog.Term.t -> Seplog.Ptos.elt
val symex_load_rule : Rule.t
val symex_store_rule : Rule.t
val symex_free_rule : Rule.t
val symex_new_rule : Rule.t
val symex_skip_rule : Rule.t
val symex_if_rule : Rule.t
val symex_ifelse_rule : Rule.t
val symex_while_rule : Rule.t

val matches :
  Seplog.Form.t * Program.Cmd.t ->
  Seplog.Form.t * Program.Cmd.t ->
  Seplog.Unify.Unidirectional.state list

val subst_rule :
  Seplog.Subst.t ->
  Seplog.Form.t * Program.Cmd.t ->
  Seplog.Form.t * Program.Cmd.t ->
  (((Seplog.Form.t * Program.Cmd.t) * Generic.Tagpairs.t * Generic.Tagpairs.t)
   list
  * string)
  list

val frame :
  Seplog.Form.t * Program.Cmd.t ->
  Seplog.Form.t * Program.Cmd.t ->
  (((Seplog.Form.t * Program.Cmd.t) * Generic.Tagpairs.t * Generic.Tagpairs.t)
   list
  * string)
  list

val dobackl : int -> Proof.t -> (int list * Proof.t) list
val fold : Seplog.Preddef.t -> Rule.t
val generalise_while_rule : Rule.t

module Slprover :
  Generic.Prover.S
    with type Seq.t = Seplog.Seq.t
     and type rule_t = Generic.Proofrule.Make(Seplog.Seq).t
     and module Proof = Generic.Proof.Make(Seplog.Seq)

val backlink_cut : Seplog.Defs.t -> Rule.t
val axioms : Rule.t ref
val rules : Rule.t ref
val symex : Rule.t
val setup : Seplog.Defs.t -> unit
