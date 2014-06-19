val split_heaps : bool ref

module Term :
sig
  include Util.BasicType 
  module Set : Util.OrderedContainer with type elt=t
  module Map : Util.OrderedMap with type key = t
  module FList : Util.BasicType with type t = t list

  val nil : t
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
  val avoid_theta : Set.t -> Set.t -> substitution 

  val to_melt : t -> Latex.t
  val parse : (t, 'a) MParser.parser
end

(* union-find type for equalitites *)
module UF :
sig
  type t
  val empty : t
  val equal : t -> t -> bool
  val compare : t -> t -> int
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
  val to_subst : t -> Term.substitution
  val subst : Term.substitution -> t -> t
  val vars : t -> Term.Set.t
  val to_string_list : t -> string list
  val to_melt : t -> Latex.t
  val equates : t -> Term.t -> Term.t -> bool
  val uni_subsumption : bool ->
    (Term.substitution -> Term.substitution option) ->
    Term.substitution -> t -> t -> Term.substitution option
  val is_subsumed : t -> t -> bool
end

(* set-like type for disequalities *)
(* it is guaranteed that for any pair (x,y) in the set, x<=y *)
module Deqs : 
sig
  include Util.OrderedContainer with type elt = Term.t * Term.t
  val parse : (Term.t * Term.t, 'a) MParser.parser
  val subst : Term.substitution -> t -> t
  val vars : t -> Term.Set.t
  val to_string_list : t -> string list
  val to_melt : t -> Latex.t
  val uni_subsumption : bool ->
    (Term.substitution -> Term.substitution option) ->
    Term.substitution -> t -> t -> Term.substitution option
end

module Ptos : 
sig
  include Util.OrderedContainer with type elt=Term.t * Term.t list
  val parse : (Term.t * Term.t list, 'a) MParser.parser
  val subst : Term.substitution -> t -> t
  val vars : t -> Term.Set.t
  val to_string_list : t -> string list
  val to_melt : t -> Latex.t
  val aux_subsumption : bool -> bool ->
    (Term.substitution -> Term.substitution option) ->
    Term.substitution -> t -> t -> Term.substitution option
end

module IndSubf : Util.MCTsig with type t = Util.Strng.t * Term.FList.t
type ind_subf = IndSubf.t

type ind_identifier = string
type ind_pred = int * (ind_identifier * Term.t list)
module Inds : 
sig
  include Util.OrderedContainer with type elt=ind_pred
  val parse : (ind_pred, 'a) MParser.parser
  val subst : Term.substitution -> t -> t
  val vars : t -> Term.Set.t
  val to_string_list : t -> string list
  val to_melt : t -> Latex.t
  val aux_subsumption : bool -> bool ->
    (Term.substitution -> Term.substitution option) ->
    Term.substitution -> t -> t -> Term.substitution option
  val subsumed_wrt_tags : Util.Tags.t -> t -> t -> bool
  val equal_wrt_tags : Util.Tags.t -> t -> t -> bool
end

exception Not_symheap

val has_ident : ind_identifier ->  ind_pred -> bool

