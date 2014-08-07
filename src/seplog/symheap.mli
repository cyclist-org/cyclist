val split_heaps : bool ref

(* set-like type for disequalities *)
(* it is guaranteed that for any pair (x,y) in the set, x<=y *)
module Deqs : 
sig
  include Util.OrderedContainer with type elt = Sl_term.t * Sl_term.t
  val parse : (Sl_term.t * Sl_term.t, 'a) MParser.parser
  val subst : Sl_term.substitution -> t -> t
  val vars : t -> Sl_term.Set.t
  val to_string_list : t -> string list
  val to_melt : t -> Latex.t
  val uni_subsumption : bool ->
    (Sl_term.substitution -> Sl_term.substitution option) ->
    Sl_term.substitution -> t -> t -> Sl_term.substitution option
  val subsumed : Sl_uf.t -> t -> t -> bool
end

module Ptos : 
sig
  include Util.OrderedContainer with type elt=Sl_term.t * Sl_term.t list
  val parse : (Sl_term.t * Sl_term.t list, 'a) MParser.parser
  val subst : Sl_term.substitution -> t -> t
  val vars : t -> Sl_term.Set.t
  val to_string_list : t -> string list
  val to_melt : t -> Latex.t
  val aux_subsumption : bool -> bool ->
    (Sl_term.substitution -> Sl_term.substitution option) ->
    Sl_term.substitution -> t -> t -> Sl_term.substitution option
  val subsumed : Sl_uf.t -> t -> t -> bool
end

module IndSubf : Util.MCTsig with type t = Util.Strng.t * Sl_term.FList.t
type ind_subf = IndSubf.t

type ind_identifier = string
type ind_pred = int * (ind_identifier * Sl_term.t list)
module Inds : 
sig
  include Util.OrderedContainer with type elt=ind_pred
  val parse : (ind_pred, 'a) MParser.parser
  val subst : Sl_term.substitution -> t -> t
  val vars : t -> Sl_term.Set.t
  val to_string_list : t -> string list
  val to_melt : t -> Latex.t
  val tags : t -> Util.Tags.t
  val freshen_tags : t -> t -> t
  val aux_subsumption : bool -> bool ->
    (Sl_term.substitution -> Sl_term.substitution option) ->
    Sl_term.substitution -> t -> t -> Sl_term.substitution option
  val subsumed_wrt_tags : Util.Tags.t -> t -> t -> bool
  (* val equal_wrt_tags : Util.Tags.t -> t -> t -> bool *)
  val subsumed : Sl_uf.t -> t -> t -> bool
end

exception Not_symheap

val has_ident : ind_identifier ->  ind_pred -> bool

