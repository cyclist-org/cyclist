open Lib

type term

module Term :
sig
  include BasicType with type t = term
  module Set : OrderedContainer with type elt = term
  module Map : OrderedMap with type key = term

  type substitution
    (* = t Map.t *)

  val empty_subst : substitution
  val singleton_subst : t -> t -> substitution
  val subst_is_identity : substitution -> bool
  val subst : substitution -> t -> t

  val pp : Format.formatter -> t -> unit

  val zero : t

  val is_zero : t -> bool
  val is_var : t -> bool
  val is_exist_var : t -> bool
  val is_free_var : t -> bool
  val is_fun : t -> bool
  val is_succ : t -> bool
  val is_cons : t -> bool

  val mk_const : int -> t
  val mk_univ_var : string -> t
  val mk_exist_var : string -> t
  val mk_fun : string -> t list -> t

  val dest_fun : t -> (string * t list)

  val vars : t -> Set.t

  val unify_list : substitution -> t list -> t list -> substitution option
  val multi_unify_args : t list -> t list -> (substitution * (t * t) list) option
end

module Atom :
sig
  type t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit

  val is_eq : t -> bool
  val is_deq : t -> bool
  val is_ipred : t -> bool

  val mk_eq : term -> term -> t
  val mk_deq : term -> term -> t
  val mk_ipred : Tags.Elt.t -> string -> term list -> t

  val dest_eq : t -> term * term
  val dest_deq : t -> term * term
  val dest_pred : t -> Tags.Elt.t * string * term list

  val ipred_eq_mod_tags : t -> t -> bool
  val vars : t -> Term.Set.t
  val tag : t -> Tags.Elt.t option
end

module Prod :
sig
  include OrderedContainer with type elt = Atom.t

  val get_eqs : t -> (term * term) list
  val get_deqs : t -> (term * term) list
  val filter_by_kind : Atom.t -> t -> t

  val tags : t -> Tags.t
  val univ : Term.Set.t -> t -> t
  val repl_tags : Tags.Elt.t -> t -> t
  val subsumed_wrt_tags : Tags.t -> t -> t -> bool
  val tag_pairs : t -> Tagpairs.t
  val subst : Term.substitution -> t -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val left_subsumption :
    (Term.substitution -> Term.substitution option) ->
    Term.substitution -> t -> t -> Term.substitution option
end

module Form :
sig
  include OrderedContainer with type elt = Prod.t
  val dest : t -> Prod.t
  val tags : t -> Tags.t
  val tag_pairs : t -> Tagpairs.t
  val pp : Format.formatter -> t -> unit
  val is_prod : t -> bool
end

exception Not_product

module Seq :
sig
  type t = Form.t * Form.t

  val equal : t -> t -> bool
  val equal_upto_tags : t -> t -> bool
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val of_string : string -> t

  val tags : t -> Tags.t
  val dest : t -> Prod.t * Prod.t
  val terms : t -> Term.Set.t
  val vars : t -> Term.Set.t
  val tag_pairs : t -> Tagpairs.t
  val uni_subsumption : t -> t -> Term.substitution option
  val subsumed_wrt_tags : Tags.t -> t -> t -> bool
  val subst : Term.substitution -> t -> t
end

module Case :
sig
  type t
  val mk : Prod.t -> term list -> t
  val dest : t -> Prod.t * term list
  val vars : t -> Term.Set.t
  val freshen : Term.Set.t -> t -> t
end

module Defs :
sig
  type t
  val empty : t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val add : string -> Case.t -> t -> t
  val bindings : t -> (string * Case.t list) list
  val of_channel : in_channel -> t
end
