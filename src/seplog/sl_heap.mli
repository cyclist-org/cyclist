type symheap = {
  eqs : Symheap.UF.t;
  deqs : Symheap.Deqs.t;
  ptos : Symheap.Ptos.t;
  inds : Symheap.Inds.t;
}
include Util.BasicType with type t = symheap

val empty : t

val get_idents : t -> Util.Strng.MSet.t
val vars : t -> Sl_term.Set.t
val terms : t -> Sl_term.Set.t
val tags : t -> Util.Tags.t
val norm : t -> t
val tag_pairs : t -> Util.TagPairs.t
val star : t -> t -> t
val univ : Sl_term.Set.t -> t -> t
val repl_tags : int -> t -> t
val equates : t -> Sl_term.t -> Sl_term.t -> bool
val disequates : t -> Sl_term.t -> Sl_term.t -> bool
val eq_class : t -> Sl_term.t -> Sl_term.Set.t
val mk_pto : Sl_term.t -> Sl_term.t list -> t
val mk_eq : Sl_term.t -> Sl_term.t -> t
val mk_deq : Sl_term.t -> Sl_term.t -> t
val mk_ind : int -> Symheap.ind_identifier -> Sl_term.t list -> t
val subst : Sl_term.substitution -> t -> t
val aux_subsumed_wrt_tags : bool -> Util.Tags.t -> t -> t -> bool
val aux_subsumption : bool -> bool ->
(Sl_term.substitution -> Sl_term.substitution option) ->
Sl_term.substitution -> t -> t -> Sl_term.substitution option
val spw_left_subsumption :
(Sl_term.substitution -> Sl_term.substitution option) ->
Sl_term.substitution -> t -> t -> Sl_term.substitution option
val find_lval : Sl_term.t -> t -> (Sl_term.t * Sl_term.t list) option
val inconsistent : t -> bool
val subst_existentials : t -> t
val is_fresh_in : Sl_term.t -> t -> bool
val fixpoint : (t -> t) -> t -> t
val parse : (t, 'a) MParser.t
val project : t -> Sl_term.t list -> t
val to_melt : t -> Latex.t
