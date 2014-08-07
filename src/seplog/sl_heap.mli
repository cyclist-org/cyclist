type symheap = private {
  eqs : Sl_uf.t;
  deqs : Symheap.Deqs.t;
  ptos : Symheap.Ptos.t;
  inds : Symheap.Inds.t;
}
include Util.BasicType with type t = symheap

val empty : t

val mk_pto : Sl_term.t -> Sl_term.t list -> t
val mk_eq : Sl_term.t -> Sl_term.t -> t
val mk_deq : Sl_term.t -> Sl_term.t -> t
val mk_ind : int -> Symheap.ind_identifier -> Sl_term.t list -> t
val mk : Sl_uf.t -> Symheap.Deqs.t -> Symheap.Ptos.t -> Symheap.Inds.t -> t

val with_eqs : t -> Sl_uf.t -> t 
val with_deqs : t -> Symheap.Deqs.t -> t
val with_ptos : t -> Symheap.Ptos.t -> t
val with_inds : t -> Symheap.Inds.t -> t

val del_deq : t -> (Sl_term.t * Sl_term.t) -> t
val del_pto : t -> (Sl_term.t * Sl_term.t list) -> t
val del_ind : t -> Symheap.ind_pred -> t

val star : t -> t -> t
val parse : (t, 'a) MParser.t
val of_string : string -> t

val subst : Sl_term.substitution -> t -> t
val norm : t -> t
val univ : Sl_term.Set.t -> t -> t
val subst_existentials : t -> t
val project : t -> Sl_term.t list -> t
val fixpoint : (t -> t) -> t -> t
val freshen_tags : t -> t -> t

val get_idents : t -> Util.Strng.MSet.t
val vars : t -> Sl_term.Set.t
val terms : t -> Sl_term.Set.t
val tags : t -> Util.Tags.t
val tag_pairs : t -> Util.TagPairs.t
val equates : t -> Sl_term.t -> Sl_term.t -> bool
val disequates : t -> Sl_term.t -> Sl_term.t -> bool
val eq_class : t -> Sl_term.t -> Sl_term.Set.t

val subsumed : t -> t -> bool

val aux_subsumption : bool -> bool ->
(Sl_term.substitution -> Sl_term.substitution option) ->
Sl_term.substitution -> t -> t -> Sl_term.substitution option
val spw_left_subsumption :
(Sl_term.substitution -> Sl_term.substitution option) ->
Sl_term.substitution -> t -> t -> Sl_term.substitution option

val find_lval : Sl_term.t -> t -> (Sl_term.t * Sl_term.t list) option
val inconsistent : t -> bool
val is_fresh_in : Sl_term.t -> t -> bool
val to_melt : t -> Latex.t
