type symheap = {
  eqs : Symheap.UF.t;
  deqs : Symheap.Deqs.t;
  ptos : Symheap.Ptos.t;
  inds : Symheap.Inds.t;
}
include Util.BasicType with type t = symheap

val empty : t

val get_idents : t -> Util.Strng.MSet.t
val vars : t -> Symheap.Term.Set.t
val terms : t -> Symheap.Term.Set.t
val tags : t -> Util.Tags.t
val norm : t -> t
val tag_pairs : t -> Util.TagPairs.t
val star : t -> t -> t
val univ : Symheap.Term.Set.t -> t -> t
val repl_tags : int -> t -> t
val equates : t -> Symheap.Term.t -> Symheap.Term.t -> bool
val disequates : t -> Symheap.Term.t -> Symheap.Term.t -> bool
val eq_class : t -> Symheap.Term.t -> Symheap.Term.Set.t
val mk_pto : Symheap.Term.t -> Symheap.Term.t list -> t
val mk_eq : Symheap.Term.t -> Symheap.Term.t -> t
val mk_deq : Symheap.Term.t -> Symheap.Term.t -> t
val mk_ind : int -> Symheap.ind_identifier -> Symheap.Term.t list -> t
val subst : Symheap.Term.substitution -> t -> t
val aux_subsumed_wrt_tags : bool -> Util.Tags.t -> t -> t -> bool
val aux_subsumption : bool -> bool ->
(Symheap.Term.substitution -> Symheap.Term.substitution option) ->
Symheap.Term.substitution -> t -> t -> Symheap.Term.substitution option
val spw_left_subsumption :
(Symheap.Term.substitution -> Symheap.Term.substitution option) ->
Symheap.Term.substitution -> t -> t -> Symheap.Term.substitution option
val find_lval : Symheap.Term.t -> t -> (Symheap.Term.t * Symheap.Term.t list) option
val inconsistent : t -> bool
val subst_existentials : t -> t
val is_fresh_in : Symheap.Term.t -> t -> bool
val fixpoint : (t -> t) -> t -> t
val parse : (t, 'a) MParser.t
val project : t -> Symheap.Term.t list -> t
val to_melt : t -> Latex.t
