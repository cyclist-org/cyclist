val split_heaps : bool ref

module Term :
sig
  type t
  module Set : Util.OrderedContainer with type elt=t
  module Map : Util.OrderedMap with type key = t

  val nil : t
  val equal : t -> t -> bool
  val list_equal : t list -> t list -> bool

  val is_nil : t -> bool
  val is_var : t -> bool
  val is_exist_var : t -> bool
  val is_univ_var : t -> bool

  val mk_exist_var : string -> t
  val mk_univ_var : string -> t
  val filter_vars : Set.t -> Set.t

  val fresh_uvar : Set.t -> t
  val fresh_uvars : Set.t -> int -> t list
  val fresh_evar : Set.t -> t
  val fresh_evars : Set.t -> int -> t list

  type substitution = t Map.t
  val empty_subst : substitution
  val singleton_subst : t -> t -> substitution
  val subst : substitution -> t -> t
  val pp_subst : Format.formatter -> substitution -> unit

  val to_string : t -> string
  val to_melt : t -> Latex.t
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val parse : (t, 'a) MParser.parser
end

(* union-find type for equalitites *)
module UF :
sig
  type t
  val empty : t
  val is_empty : t -> bool
  val find : Term.t -> t -> Term.t
  val add : Term.t * Term.t -> t -> t
  val union : t -> t -> t
  (* if a pair contains an exist. variable then *)
  (* the first comp of the pair is an exist. var *)
  val bindings : t -> (Term.t * Term.t) list
  val of_list : (Term.t * Term.t) list -> t
	val remove: Term.t -> t -> t
  val parse : (Term.t * Term.t, 'a) MParser.parser
end

(* set-like type for disequalities *)
(* it is guaranteed that for any pair (x,y) in the set, x<=y *)
module Deqs : 
sig
  include Util.OrderedContainer with type elt = Term.t * Term.t
  val parse : (Term.t * Term.t, 'a) MParser.parser
end

module Ptos : 
sig
  include Util.OrderedContainer with type elt=Term.t * Term.t list
  val parse : (Term.t * Term.t list, 'a) MParser.parser
end

type ind_identifier = string
type ind_pred = int * (ind_identifier * Term.t list)
module Inds : 
sig
  include Util.OrderedContainer with type elt=ind_pred
  val parse : (ind_pred, 'a) MParser.parser
end

type symheap = {
  eqs : UF.t;
  deqs : Deqs.t;
  ptos : Ptos.t;
  inds : Inds.t;
}
module Heap :
sig
  type t = symheap
  val empty : t

  val get_idents : t -> Util.Strng.MSet.t
  val vars : t -> Term.Set.t
  val terms : t -> Term.Set.t
  val tags : t -> Util.Tags.t
  val norm : t -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val tag_pairs : t -> Util.TagPairs.t
  val star : t -> t -> t
  val univ : Term.Set.t -> t -> t
  val repl_tags : int -> t -> t
  val equates : t -> Term.t -> Term.t -> bool
  val disequates : t -> Term.t -> Term.t -> bool
	val eq_class : t -> Term.t -> Term.Set.t
  val compare : t -> t -> int
  val mk_pto : Term.t -> Term.t list -> t
  val mk_eq : Term.t -> Term.t -> t
  val mk_deq : Term.t -> Term.t -> t
  val mk_ind : int -> ind_identifier -> Term.t list -> t
  val subst : Term.substitution -> t -> t
  val spw_left_subsumption :
    (Term.substitution -> Term.substitution option) ->
    Term.substitution -> t -> t -> Term.substitution option
  val find_lval : Term.t -> t -> (Term.t * Term.t list) option
  val equal : t -> t -> bool
  val inconsistent : t -> bool
  val subst_existentials : t -> t
  val is_fresh_in : Term.t -> t -> bool
  val fixpoint : (t -> t) -> t -> t
  val parse : (t, 'a) MParser.t
end

module Form :
sig
  type t = symheap list
  val empty : t
  val dest : t -> Heap.t

  val star : t -> t -> t
  val disj : t -> t -> t
  val to_string : t -> string
  val to_melt : t -> Latex.t
  val pp : Format.formatter -> t -> unit
  val norm : t -> t
  val equal : t -> t -> bool
  val terms : t -> Term.Set.t
  val vars : t -> Term.Set.t
  val tags : t -> Util.Tags.t
  val tag_pairs : t -> Util.TagPairs.t
  val equates : t -> Term.t -> Term.t -> bool
  val inconsistent : t -> bool
  val subst : Term.substitution -> t -> t
  val subsumed_wrt_tags : Util.Tags.t -> t -> t -> bool
  val spw_subsumed_wrt_tags : Util.Tags.t -> t -> t -> bool
  val left_subsumption :
    (Term.substitution -> Term.substitution option) ->
    Term.substitution -> t -> t -> Term.substitution option
    (* spatial weakening version *)
  val spw_left_subsumption :
    (Term.substitution -> Term.substitution option) ->
    Term.substitution -> t -> t -> Term.substitution option
  val subst_existentials : t -> t
  val is_fresh_in : Term.t -> t -> bool
  val is_heap : t -> bool
  val parse : (t, 'a) MParser.t
end
exception Not_symheap

module Seq :
sig
  type t = Form.t * Form.t

  val dest : t -> Heap.t * Heap.t
  val to_string : t -> string
  val to_melt : t -> Latex.t
  val vars : t -> Term.Set.t
  val tags : t -> Util.Tags.t
  val tag_pairs : t -> Util.TagPairs.t
  val subst : Term.substitution -> t -> t
  val subsumed_wrt_tags : Util.Tags.t -> t -> t -> bool
  val uni_subsumption : t -> t -> Term.substitution option
  val norm : t -> t
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val parse : (t, 'a) MParser.t
  val of_string : string -> t
end

module Case :
sig
  type t
  val mk : Heap.t -> ind_identifier * Term.t list -> t
  val dest: t -> Heap.t * (ind_identifier * Term.t list)
  val vars : t -> Term.Set.t
  val freshen : Term.Set.t -> t -> t
end

module Defs :
sig
  type t = (Case.t list * ind_identifier) list
	val equal : t -> t -> bool
  val fixpoint: (t -> t) -> t -> t
	
  val to_string : t -> string
  val to_melt : t -> Latex.t
  val pp : Format.formatter -> t -> unit

  (* val empty : t *)
  (* val add_case : Case.t -> t -> t *)
  (* val predicates : t -> string list *)
  (* val arity : string -> t -> int *)
  val mem : string -> t -> bool
  val is_defined : ind_pred -> t -> bool
  val get_def : string -> t -> (Case.t list * ind_identifier)

  val consistent : t -> bool -> bool -> bool
  val parse : (t, 'a) MParser.t
  val of_channel : in_channel -> t
end

val has_ident : ind_identifier ->  ind_pred -> bool

